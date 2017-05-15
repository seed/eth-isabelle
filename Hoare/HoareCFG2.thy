theory "HoareCFG2"

imports "Hoare"
begin
type_synonym position = "int * int"
type_synonym pred = "(position state_element set \<Rightarrow> bool)"

definition cfg_triple ::
 "failure_reason set \<Rightarrow> ('a state_element set \<Rightarrow> bool) \<Rightarrow> ('a * inst) list \<Rightarrow> ('a state_element set \<Rightarrow> bool) \<Rightarrow> bool"
where
  "cfg_triple allowed_failures pre insts post ==
     \<forall> co_ctx presult rest stopper. no_assertion co_ctx \<longrightarrow>
       (pre ** code (set insts) ** rest) (instruction_result_as_set co_ctx presult) \<longrightarrow>
         (((post ** code (set insts) ** rest) (instruction_result_as_set co_ctx (program_sem stopper co_ctx (length insts) presult)))
         \<or> failed_for_reasons allowed_failures (program_sem stopper co_ctx (length insts) presult))"

text {* We define an inductive system that define a Hoare triple verified by one basic block of the CFG.
The considered basic blocks is the one pointed by the program counter of the pre-condition.
Here we assume that we always are at the beginning of a block.
The post-condition holds after executing the basic block + the transition to the next block. *}
inductive
  run_one :: "cfg \<Rightarrow> pred \<Rightarrow> pred \<Rightarrow> bool" 
where
  one_no: "\<lbrakk> Some (No,co) = cfg_blocks c n; cfg_triple {} (program_counter (n,0) ** pre) co post\<rbrakk> 
  \<Longrightarrow> run_one c (program_counter (n,0) ** pre) post"
| one_next : "
 \<lbrakk>Some (i, _) = cfg_edges c n;
  Some (Next,co) = cfg_blocks c n;
  cfg_triple {} (program_counter (n,0) ** continuing ** rest_pre)
              co
             (program_counter (n,m) ** continuing ** rest_post)\<rbrakk>
  \<Longrightarrow> run_one c (program_counter (n,0) ** continuing ** rest_pre)
                (program_counter (i,0) ** continuing ** rest_post)"
| one_jump : "
\<lbrakk>Some (i, _) = cfg_edges c n;
  Some (Jump,co) = cfg_blocks c n;
  cfg_triple {} (program_counter (n,0) ** continuing ** rest_pre)
              co
             (program_counter (n,m) ** continuing ** stack_height (Suc h) ** gas_pred a ** stack h x ** rest)\<rbrakk>
  \<Longrightarrow> run_one c (program_counter (n,0) ** continuing ** rest_pre)
                (program_counter (i,0) ** continuing ** stack_height h ** gas_pred (a - Gmid) **  rest)"
| one_jumpi_true : "
 \<lbrakk>Some (i, _) = cfg_edges c n;
  Some (Jumpi,co) = cfg_blocks c n;
  y \<noteq> 0;
  cfg_triple {} (program_counter (n,0) ** continuing ** rest_pre)
              co
             (program_counter (n,m) ** continuing ** stack_height (Suc(Suc h)) ** gas_pred a ** stack (Suc h) x ** stack h y ** rest)\<rbrakk>
  \<Longrightarrow> run_one c (program_counter (n,0) ** continuing ** rest_pre)
                (program_counter (i,0) ** continuing ** stack_height h ** gas_pred (a - Ghigh) ** rest)"
| one_jumpi_false : "
 \<lbrakk>Some (_, Some j) = cfg_edges c n;
  Some (Jumpi,co) = cfg_blocks c n;
  y = 0;
  cfg_triple {} (program_counter (n,0) ** continuing ** rest_pre)
              co
             (program_counter (n,m) ** continuing ** stack_height (Suc(Suc h)) ** gas_pred a ** stack (Suc h) x ** stack h y ** rest)\<rbrakk>
  \<Longrightarrow> run_one c (program_counter (n,0) ** continuing ** rest_pre)
                (program_counter (j,0) ** continuing ** stack_height h ** gas_pred (a - Ghigh) ** rest)"
| one_pre  : "run_one c pre1 post \<Longrightarrow>  (\<forall>s. pre2 s \<longrightarrow> pre1 s)  \<Longrightarrow> run_one c pre2 post"
| one_post : "run_one c pre post1 \<Longrightarrow> (\<forall>s. post1 s \<longrightarrow> post2 s) \<Longrightarrow> run_one c pre post2"

text {* We define an inductive system that define a Hoare triple verified by executing a part of the CFG.
The first considered basic blocks is the one pointed by the program counter of the pre-condition.
Here we assume that we always are at the beginning of a block.
The post-condition holds after executing the basic block + all the other basic blocks that follow the first in the execution order. *}
inductive
  run_all :: "cfg \<Rightarrow> pred \<Rightarrow> pred \<Rightarrow> bool"
where
  all_end : "\<lbrakk> Some (No,_) = cfg_blocks c n; run_one c (program_counter (n,0) ** pre) post\<rbrakk> 
  \<Longrightarrow> run_all c (program_counter (n,0) ** pre) post"
| all_continue : "\<lbrakk>Some (ty,_) = cfg_blocks c n; ty \<noteq> No ; run_one c (program_counter (n,0) ** pre) q; run_all c q post\<rbrakk>
  \<Longrightarrow> run_all c (program_counter (n,0) ** pre) post"

(* Properties *)

lemma cfg_triple_implies :
"cfg_triple f pre l post \<Longrightarrow> triple f pre (set l) post"
apply(auto simp add: cfg_triple_def triple_def code_middle shuffle3)
apply(rule_tac x="length l" in exI; simp)
done

lemma empty_triple:
"(\<forall>s. p s \<Longrightarrow> r s) \<Longrightarrow> cfg_triple {} p [] r"
apply(simp add: cfg_triple_def program_sem.simps failed_for_reasons_def)
apply auto
sorry

lemma composition_triple:
"cfg_triple f p xs r \<Longrightarrow> cfg_triple f r ys q \<Longrightarrow> cfg_triple f p (xs@ys) q"
apply(auto simp add: cfg_triple_def code_middle shuffle3)
apply(drule_tac x = "co_ctx" in spec; simp)
apply(drule_tac x = "presult" in spec)
apply(drule_tac x = co_ctx in spec; simp)
apply(drule_tac x = "code (set ys) ** rest" in spec; simp add: code_more)
sorry


(* Example *)
definition c where
"c = build_cfg [Stack (PUSH_N [1]), Stack (PUSH_N [0x6]), Pc JUMP, Misc STOP, Pc JUMPDEST, Misc STOP]"

lemmas evm_fun_simps = inst_stack_numbers.simps stack_inst_code.simps inst_size_def inst_code.simps 
pc_inst_numbers.simps 
misc_inst_numbers.simps

lemmas cfg_def = build_cfg_def update_edges_def byteListInt_def find_block_def deconstruct_def extract_indexes_def

schematic_goal c_val:
 " c = ?p"
apply(simp add: c_def  word_rcat_def bin_rcat_def Let_def evm_fun_simps cfg_def
  split:if_splits nat.splits option.splits)
done

lemma " run_all c
(program_counter (0,0) ** continuing ** stack_height 0 ** gas_pred 1000)
(stack 0 (word_rcat [1::byte]) ** program_counter (6,1))
"
apply(unfold c_val)
apply(rule all_continue)
 apply(simp)
 apply(simp)
apply(rule one_jump[where m=4 and h="Suc 0" and a="1000 - 2 * Gverylow" and x="word_rcat [6]"
    and rest="stack 0 (word_rcat [1])"])
 apply(simp)
    apply(simp)
   apply (clarsimp simp add: cfg_triple_def)
    apply (sep_select 3)
    apply (sep_select_asm 3)
defer
apply(rule all_end[where n=6])
 apply(simp)
apply(rule one_no)
 apply(simp)
defer
apply(simp add: cfg_triple_def)
sorry
end
