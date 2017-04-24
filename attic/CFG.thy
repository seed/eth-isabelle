theory "CFG"

imports "../lem/Evm" "Parse"

begin
(* Types definitions *)
datatype tblock =
Next | Jump | Jumpi | No

type_synonym vertice = "int * tblock * inst list"
type_synonym vertices = "vertice list"

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

end