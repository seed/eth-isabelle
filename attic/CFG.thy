theory "CFG"

imports "../lem/Evm" "Parse"

begin
(* Types definitions *)
datatype tblock =
Next | Jump | Jumpi | No

type_synonym vertice = "int * tblock * inst list"
type_synonym vertices = "vertice list"
type_synonym edge = "int * (int option)"
type_synonym edges = "int \<Rightarrow> edge option"

datatype stack_value =
Value "int" | Data

datatype edge_return = 
NEXT | GOTO "int" | GOTOIF "int" | UNDEFINED "char list" | NONE

datatype edges_return = 
Complete "edges" | Incomplete "edges * char list * stack_value list"

(* Auxiliary functions *)

definition byteListInt :: "8 word list \<Rightarrow> int" where
"byteListInt l = uint ((word_rcat l):: 32 word)"

abbreviation scnd :: "'a * 'b * 'c \<Rightarrow> 'b" where
"scnd v == fst (snd v)"

abbreviation thrd :: "'a * 'b * 'c \<Rightarrow> 'c" where
"thrd v == snd (snd v)"

fun next_i :: "vertices \<Rightarrow> int \<Rightarrow> int" where
  "next_i (i#j#l) n = (if fst i=n then fst j else next_i (j#l) n)"

definition find_block :: "int \<Rightarrow> vertice \<Rightarrow> bool" where
"find_block n bl = (if n=fst bl then True else False)"

fun good_dest :: "int \<Rightarrow> vertices \<Rightarrow> bool" where
  "good_dest m [] = False"
| "good_dest m ((n,_,[])#l) = good_dest m l"
| "good_dest m ((n,_, i#inst)#l) = (if m = n then (if i = Pc JUMPDEST then True else False) else good_dest m l )"

fun not_complete :: "edges_return \<Rightarrow> char list \<Rightarrow> stack_value list\<Rightarrow> edges_return " where
  "not_complete (Complete edges) debug st= Incomplete (edges,debug,st)"
| "not_complete res debug st= res"

definition update_edges :: "edges_return \<Rightarrow> int \<Rightarrow> edge \<Rightarrow> edges_return" where
"update_edges e i v = (case e of
  Complete m \<Rightarrow> Complete (m(i:= Some v))
| Incomplete (m,d,s) \<Rightarrow> Incomplete (m(i:= Some v), d,s)
)"

definition concat_map :: "int \<Rightarrow> edge \<Rightarrow> edges_return \<Rightarrow> edges_return \<Rightarrow> edges_return" where
  "concat_map n v m1 m2 = (case (m1,m2) of 
  (Complete e1, Complete e2) \<Rightarrow> Complete ((e1 ++ e2)(n:= Some v))
| (Complete e1, Incomplete (e2,d,s)) \<Rightarrow> Incomplete ((e1 ++ e2)(n:= Some v),d,s)
| (Incomplete (e1,d,s), Complete e2) \<Rightarrow> Incomplete ((e1 ++ e2)(n:= Some v),d,s)
| (Incomplete (e1,d1,s1), Incomplete (e2,d2,s2)) \<Rightarrow> Incomplete ((e1 ++ e2)(n:= Some v),d1,s1)
)"

fun extract_indexes :: "vertices \<Rightarrow> int list" where
  "extract_indexes [] = []"
| "extract_indexes ((i,b)#a) = i # (extract_indexes a)"

(* Stack manipulations *)

definition stack_swap :: "stack_value list \<Rightarrow> nat \<Rightarrow> stack_value list" where
"stack_swap st n = (let first = hd st in 
  let unchanged = take (n -2) (tl st) in
  let to_swap = hd (drop (max (n-1) 1) st) in
  (to_swap # unchanged) @ (first # (drop n st))
)"

value "stack_swap [Value 1, Value 2, Value 3, Value 4] 2"

definition stack_dup :: "stack_value list \<Rightarrow> nat \<Rightarrow> stack_value list" where
"stack_dup st n = (st ! (n-1)) # st"

value "stack_dup [Value 1, Value 2, Value 3, Value 4] 2"

(* Main functions *)

(* The execution of a basic block must be sequential. *)
(* We remove JUMP and JUMPI instructions and cut after them or a stopping instrction *)
(* and before a Jump destination. *)
fun aux_basic_block :: "inst list \<Rightarrow> int \<Rightarrow> int \<Rightarrow> inst list \<Rightarrow> vertices" where
 "aux_basic_block [] pointer block_pt block = (if block = [] then [] else
    [(block_pt, No, rev block)])"
|"aux_basic_block ((i)#tl1) pointer block_pt block = 
  (let newpointer = pointer + (inst_size i) in
  (case i of
    Pc JUMPDEST \<Rightarrow> (if block = [] then (aux_basic_block tl1 newpointer pointer [i])
    else (block_pt, Next, rev block) # (aux_basic_block tl1 newpointer pointer [i]))
  | Pc JUMP \<Rightarrow>(block_pt, Jump, rev block) # ( aux_basic_block tl1 newpointer pointer [])
  | Pc JUMPI \<Rightarrow>(block_pt, Jumpi, rev block) # ( aux_basic_block tl1 newpointer pointer [])
  | Misc RETURN \<Rightarrow>(block_pt, No, rev (i#block)) # ( aux_basic_block tl1 newpointer pointer [])
  | Misc SUICIDE \<Rightarrow>(block_pt, No, rev (i#block)) # ( aux_basic_block tl1 newpointer pointer [])
  | Misc STOP \<Rightarrow>(block_pt, No, rev (i#block)) # ( aux_basic_block tl1 newpointer pointer [])
  | _ \<Rightarrow> aux_basic_block tl1 newpointer block_pt (i#block)))"

abbreviation build_basic_blocks :: "inst list \<Rightarrow> vertices" where
"build_basic_blocks prog == aux_basic_block prog 0 0 []"


(* Read a block *)
fun edge_one_block :: "tblock \<Rightarrow> inst list \<Rightarrow> stack_value list \<Rightarrow> (edge_return * stack_value list)" where
  "edge_one_block Next [] st = (NEXT, st)"
| "edge_one_block Jump [] [] = (UNDEFINED ''JUMP : Empty stack'',[])"
| "edge_one_block Jump [] (a#st) = (case a of
      Value add \<Rightarrow> (GOTO add, st)
    | Data \<Rightarrow> (UNDEFINED ''JUMP : Data on top of the stack'',a#st))"
| "edge_one_block Jumpi [] [] = (UNDEFINED ''JUMPI : Empty stack'',[])"
| "edge_one_block Jumpi [] [a] = (UNDEFINED ''JUMPI : Just 1 elet on stack'',[a])"
| "edge_one_block Jumpi [] (a#b#st) = (case a of
      Value add \<Rightarrow> (GOTOIF add,st)
    | Data \<Rightarrow> (UNDEFINED ''JUMPI : Data on top of the stack'',a#st))"
| "edge_one_block No [] st = (NONE,st)"
| "edge_one_block t ((Stack (PUSH_N data))#bl) st = edge_one_block t bl ((Value (byteListInt data))#st)"
| "edge_one_block t ((Swap i)#bl) st = (let min_height = unat i + 1 in
    (if min_height>length st 
     then (UNDEFINED ''Swap : stack too small'',st)
     else edge_one_block t bl (stack_swap st min_height)))"
| "edge_one_block t ((Dup i)#bl) st = (let min_height = unat i in
    (if min_height>length st 
     then (UNDEFINED ''Dup : stack too small'',st)
     else edge_one_block t bl (stack_dup st min_height)))"
| "edge_one_block t (i#bl) st = (let st_nb= (inst_stack_numbers i) in
    (if (nat (fst st_nb))>length st 
     then (UNDEFINED ''Stack too small'',st)
     else edge_one_block t bl ((replicate (nat (snd st_nb)) Data)@(drop (nat (fst st_nb)) st))))"



fun edges_blocks :: "int list \<Rightarrow> int \<Rightarrow> stack_value list \<Rightarrow> vertices \<Rightarrow> edges_return " where
"edges_blocks to_do n st vertices = (let new_to_do = removeAll n to_do in
  (case length to_do > length new_to_do of
    False \<Rightarrow> Complete Map.empty
  | True \<Rightarrow> (let block = find (find_block n) vertices in (case block of
        None \<Rightarrow> Incomplete (Map.empty, ''Block not found'',st)
      | Some bl \<Rightarrow>
      (let res = edge_one_block (scnd bl) (thrd bl) st in (case fst res of
        UNDEFINED s \<Rightarrow> Incomplete (Map.empty, s, st)
      | NONE \<Rightarrow> Complete Map.empty
      | NEXT \<Rightarrow> (let m = next_i vertices n in
        update_edges (edges_blocks new_to_do m (snd res) vertices) n (m, None))
      | GOTO i \<Rightarrow> (if good_dest i vertices
        then update_edges (edges_blocks new_to_do i (snd res) vertices) n (i, None)
        else Incomplete (Map.empty, ''Bad destination for JUMP'',st))
      | GOTOIF i \<Rightarrow>(let m = next_i vertices n in
        (if good_dest i vertices
        then concat_map n (i, Some m) (edges_blocks new_to_do m (snd res) vertices) (edges_blocks new_to_do i (snd res) vertices)
        else not_complete (update_edges (edges_blocks new_to_do m (snd res) vertices) n (i, Some m)) ''Bad destination for JUMPI'' st))
))))))"

definition build_cfg :: "inst list \<Rightarrow> (vertices * edges_return)" where
"build_cfg prog = (let res = build_basic_blocks prog in
(res, edges_blocks (extract_indexes res) 0 [] res ))"

(* Verification *)

(* Check that we can rebuild the initial list of instructions from basic blocks *)
fun reconstruct_bytecode :: "vertices \<Rightarrow> inst list" where
 "reconstruct_bytecode [] = []"
| "reconstruct_bytecode ((n,Jump,b)#q) = b@[Pc JUMP] @ (reconstruct_bytecode q)" 
| "reconstruct_bytecode ((n,Jumpi,b)#q) = b@[Pc JUMPI] @ (reconstruct_bytecode q)" 
| "reconstruct_bytecode ((n,_,b)#q) = b @ (reconstruct_bytecode q)" 

lemma rev_basic_blocks: "reconstruct_bytecode (aux_basic_block i p bp b) = (rev b)@i"
apply(induction i arbitrary: p bp b)
apply(auto simp: Let_def split: inst.split misc_inst.split pc_inst.split)
done

theorem reverse_basic_blocks: "reconstruct_bytecode (build_basic_blocks i) = i"
apply(simp add: rev_basic_blocks)
done

end