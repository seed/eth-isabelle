theory "CFG"

imports "../lem/Evm"

begin
(* Types definitions *)
datatype tblock =
Next | Jump | Jumpi | No

type_synonym position = "int *int"
type_synonym pos_inst = "position * inst"
type_synonym vertex = "int * pos_inst list * tblock"
type_synonym vertices = "vertex list"
type_synonym edge = "int * (int option)"
type_synonym edges = "int \<Rightarrow> edge option"

datatype stack_value =
Value "int" | Data

datatype edge_return = 
NEXT | GOTO "int" | GOTOIF "int" | UNDEFINED "char list" | NONE

datatype edges_return = 
Complete "edges" | Incomplete "edges * char list * stack_value list"

record cfg = 
cfg_indexes :: "int list"
cfg_blocks :: "int \<Rightarrow> vertex option"
cfg_edges :: "edges"

(* Auxiliary functions *)

abbreviation v_ind :: "vertex \<Rightarrow> int" where
"v_ind v == fst v"

abbreviation v_ty :: "vertex \<Rightarrow> tblock" where
"v_ty v == snd (snd v)"

abbreviation v_insts :: "vertex \<Rightarrow> pos_inst list" where
"v_insts v == fst (snd v)"

definition byteListInt :: "8 word list \<Rightarrow> int" where
"byteListInt l = uint ((word_rcat l):: 32 word)"

definition next_i :: "vertices \<Rightarrow> int \<Rightarrow> int" where
 "next_i v n = v_ind (hd (dropWhile (\<lambda>u. (v_ind u)\<le>n) v))"
(*  "next_i (i#j#l) n = (if fst i=n then fst j else next_i (j#l) n)"*)

definition find_block :: "int \<Rightarrow> vertex \<Rightarrow> bool" where
"find_block n bl = (if n=v_ind bl then True else False)"

fun good_dest :: "int \<Rightarrow> vertices \<Rightarrow> bool" where
  "good_dest m [] = False"
| "good_dest m ((n,[],_)#l) = good_dest m l"
| "good_dest m ((n,(_,i)#inst,_)#l) = (if m = n then (if i = Pc JUMPDEST then True else False) else good_dest m l )"

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

definition extract_indexes :: "vertices \<Rightarrow> int list" where
"extract_indexes xs = map v_ind xs"

definition deconstruct :: "edges_return \<Rightarrow> edges" where
"deconstruct e = (case e of (Complete i) \<Rightarrow> i | (Incomplete (i,d,s)) \<Rightarrow> i)"

fun print_edges_aux ::"int list \<Rightarrow> edges \<Rightarrow> (int * edge) list" where
"print_edges_aux [] e = []"
| "print_edges_aux (n#q) e = (case e n of None \<Rightarrow> print_edges_aux q e | Some i \<Rightarrow> (n,i) # (print_edges_aux q e))"

abbreviation print_edges :: "vertices \<Rightarrow> edges_return \<Rightarrow> (int * edge) list" where
"print_edges v e == print_edges_aux (extract_indexes v) (deconstruct e)"

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
fun aux_basic_block :: "inst list \<Rightarrow> int \<Rightarrow> int \<Rightarrow> pos_inst list \<Rightarrow> vertices" where
 "aux_basic_block [] pointer block_pt block = (if block = [] then [] else
    [(block_pt, rev block, No)])"
|"aux_basic_block ((i)#tl1) pointer block_pt block = 
  (let newpointer = pointer + (inst_size i) in
  (let pos = (block_pt, pointer - block_pt) in
  (case i of
    Pc JUMPDEST \<Rightarrow> (if block = [] then (aux_basic_block tl1 newpointer pointer [((pointer,0),i)])
    else (block_pt, rev block, Next) # (aux_basic_block tl1 newpointer pointer [((pointer,0),i)]))
  | Pc JUMP \<Rightarrow>(block_pt, rev block, Jump) # ( aux_basic_block tl1 newpointer newpointer [])
  | Pc JUMPI \<Rightarrow>(block_pt, rev block, Jumpi) # ( aux_basic_block tl1 newpointer newpointer [])
  | Misc RETURN \<Rightarrow>(block_pt, rev ((pos,i)#block), No) # ( aux_basic_block tl1 newpointer newpointer [])
  | Misc SUICIDE \<Rightarrow>(block_pt, rev ((pos,i)#block), No) # ( aux_basic_block tl1 newpointer newpointer [])
  | Misc STOP \<Rightarrow>(block_pt, rev ((pos,i)#block), No) # ( aux_basic_block tl1 newpointer newpointer [])
  | _ \<Rightarrow> aux_basic_block tl1 newpointer block_pt ((pos,i)#block))))"

abbreviation build_basic_blocks :: "inst list \<Rightarrow> vertices" where
"build_basic_blocks prog == aux_basic_block prog 0 0 []"

(* Read a block *)
fun edge_one_block :: "tblock \<Rightarrow> pos_inst list \<Rightarrow> stack_value list \<Rightarrow> (edge_return * stack_value list)" where
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
| "edge_one_block t ((_,(Stack (PUSH_N data)))#bl) st = edge_one_block t bl ((Value (byteListInt data))#st)"
| "edge_one_block t ((_,Swap i)#bl) st = (let min_height = unat i + 1 in
    (if min_height>length st 
     then (UNDEFINED ''Swap : stack too small'',st)
     else edge_one_block t bl (stack_swap st min_height)))"
| "edge_one_block t ((_,Dup i)#bl) st = (let min_height = unat i in
    (if min_height>length st 
     then (UNDEFINED ''Dup : stack too small'',st)
     else edge_one_block t bl (stack_dup st min_height)))"
| "edge_one_block t ((_,i)#bl) st = (let st_nb= (inst_stack_numbers i) in
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
      (let res = edge_one_block (v_ty bl) (v_insts bl) st in (case fst res of
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

definition build_cfg :: "inst list \<Rightarrow> cfg" where
"build_cfg prog = (let blocks = build_basic_blocks prog in
(let ind = (extract_indexes blocks) in
(let edges = deconstruct (edges_blocks ind 0 [] blocks) in
(|cfg_indexes = ind,
cfg_blocks = map_of (map (\<lambda>(n,i,t). (n,n,i,t)) blocks),
cfg_edges = edges |)
)))"

(* Verification *)

(* Check that we can rebuild the initial list of instructions from basic blocks *)
fun reconstruct_bytecode :: "vertices \<Rightarrow> inst list" where
 "reconstruct_bytecode [] = []"
| "reconstruct_bytecode ((n,b,Jump)#q) = (map snd b)@[Pc JUMP] @ (reconstruct_bytecode q)" 
| "reconstruct_bytecode ((n,b,Jumpi)#q) = (map snd b)@[Pc JUMPI] @ (reconstruct_bytecode q)" 
| "reconstruct_bytecode ((n,b,_)#q) = (map snd b) @ (reconstruct_bytecode q)" 

lemma rev_basic_blocks: "reconstruct_bytecode (aux_basic_block i p bp b) = (map snd (rev b))@i"
apply(induction i arbitrary: p bp b)
apply(auto simp: Let_def split: inst.split misc_inst.split pc_inst.split)
done

theorem reverse_basic_blocks: "reconstruct_bytecode (build_basic_blocks i) = i"
apply(simp add: rev_basic_blocks)
done

(* subsection {* Build `position program` for a control flow graph *} *)

definition empty_cfg  :: " position program "  where 
     " empty_cfg = ( (|
  program_content = (\<lambda> _ .  None),
  program_length =(( 0 :: int)),
  program_annotation = (\<lambda> _ .  []),
  program_advance_pc = (\<lambda>n.\<lambda>_. n),
  program_next_block = (\<lambda>n.\<lambda>_. n),
  program_pc_as_int = (\<lambda>_. 0),
  program_pos_from_int = (\<lambda>_. (0,0)),
  program_zero = (0,0)
|) )"

definition cfg_advance_pc :: "position \<Rightarrow> int \<Rightarrow> position" where
"cfg_advance_pc = (\<lambda>(n,m). \<lambda>i. (n, m+i))"

definition cfg_next_block :: "cfg \<Rightarrow> position \<Rightarrow> int \<Rightarrow> position" where
"cfg_next_block c pos _ = (case cfg_edges c (fst pos) of
  None \<Rightarrow> pos
| Some (m,_) \<Rightarrow> (m,0)
)"

definition cfg_pc_as_int :: "position \<Rightarrow> int" where
"cfg_pc_as_int = (\<lambda>(n,m). n + m)"

definition cfg_pos_from_int :: "cfg \<Rightarrow> int \<Rightarrow> position" where
"cfg_pos_from_int c n = (let rev_ind = rev (cfg_indexes c) in 
  (case (dropWhile (\<lambda>u. u > n) rev_ind) of
  [] \<Rightarrow> (0,0) (*Should not happen*)
 |t#q \<Rightarrow> (t,n-t)
 ))"

definition cfg_zero :: "position" where
"cfg_zero = (0,0)"

definition cfg_length :: "cfg \<Rightarrow> int " where
"cfg_length c = 
  (case cfg_blocks c (last (cfg_indexes c)) of
    None \<Rightarrow> 0 (*Should not happen*)
  | Some (_,i,Jump) \<Rightarrow> (let (n,m) = fst (last i) in n + m + 1)
  | Some (_,i,Jumpi) \<Rightarrow> (let (n,m) = fst (last i) in n + m + 1)
  | Some (_,i,_) \<Rightarrow> (let (n,m) = fst (last i) in n + m ))"

definition cfg_content :: "cfg \<Rightarrow> position \<Rightarrow> inst option" where
"cfg_content c = (\<lambda>(n,m). 
  (case cfg_blocks c n of
    None \<Rightarrow> None
  | Some b \<Rightarrow> (case find (\<lambda>(pos,_). pos=(n,m)) (v_insts b) of
      None \<Rightarrow> None
    | Some (_,i) \<Rightarrow> Some i )))"

definition program_of_cfg :: "cfg \<Rightarrow> position program " where
"program_of_cfg c = 
  program_of_lst cfg_advance_pc (cfg_next_block c) cfg_pc_as_int (cfg_pos_from_int c) cfg_zero cfg_length cfg_content c"

end