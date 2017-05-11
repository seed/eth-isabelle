theory "HoareCFG"

imports "Hoare"

begin
type_synonym position = "int * int"
type_synonym pred = "(position state_element set \<Rightarrow> bool)"

definition blocktype :: "tblock \<Rightarrow> position state_element set \<Rightarrow> bool" where
"blocktype ty s == s = {BlockTypeElm ty}"

inductive inst_rule :: "pred \<Rightarrow> pos_inst \<Rightarrow> pred \<Rightarrow> bool" where
push_n : "inst_rule (\<langle> h \<le> 1023 \<and> length lst > 0 \<and> 32 \<ge> length lst\<rangle> **
                       program_counter (k,m) **
                       continuing **
                       stack_height h **
                       gas_pred g **
                       blocktype t **
                       rest
                      )
                      ((k,m), Stack (PUSH_N lst))
                      (program_counter (k,m + 1 + (int (length lst))) **
                       continuing **
                       stack_height (Suc h) **
                       gas_pred (g - Gverylow) **
                       blocktype t **
                       stack h (word_rcat lst) ** rest
                      )"
| stop : "inst_rule 
          (\<langle> h \<le> 1024 \<rangle> ** program_counter k ** continuing ** stack_height h ** rest)
          (k, Misc STOP)
          (program_counter k ** stack_height h ** not_continuing ** action (ContractReturn []) ** rest)"
| jumpdest : "inst_rule (\<langle> h \<le> 1024 \<rangle> **
                       program_counter (k,m) **
                       continuing ** 
                       stack_height h **
                       gas_pred g **
                       rest
                      )
                      ((k,m), Pc JUMPDEST)
                      (program_counter (k,m + 1) **
                       continuing **
                       stack_height h **
                       gas_pred (g - Gjumpdest) **
                       rest
                      )"
| inst_rule_weaken_pre:
 "inst_rule p i q \<Longrightarrow> (\<And>s. r s \<Longrightarrow> p s) \<Longrightarrow> inst_rule r i q"

inductive
  bl :: "cfg \<Rightarrow> pred \<Rightarrow> (pos_inst) list \<Rightarrow> pred \<Rightarrow> bool" 
where
  empty : " bl c ((blocktype No)** rest ** common) [] common"(*to improve*)
| in_block : "\<lbrakk>inst_rule p x q; bl c q xs r\<rbrakk> \<Longrightarrow> bl c p (x#xs) r"
| next_transition : "
\<lbrakk> Some (n, _) = cfg_edges c i;
  Some (ty,co) = cfg_blocks c n;
      bl c (program_counter (n,0) ** continuing ** rest0 ** (blocktype ty) ** rest) co q \<rbrakk>
  \<Longrightarrow> bl c (program_counter (i,j) ** continuing ** rest0 ** (blocktype Next) ** rest) [] q" 
| jump_transition : "
\<lbrakk> Some (n, _) = cfg_edges c i;
  Some (ty,co) = cfg_blocks c n;
      bl c (program_counter (n,0) ** continuing ** stack_height h ** gas_pred a ** blocktype ty **  rest) co q \<rbrakk>
  \<Longrightarrow> bl c (program_counter (i,j) ** continuing ** stack_height (Suc h) ** gas_pred a ** blocktype Jump ** stack h x ** rest) [] q" 
| jumpi_transition : "
\<lbrakk> Some (n1, Some n2) = cfg_edges c i;
  Some (ty1,co1) = cfg_blocks c n1;
  Some (ty2,co2) = cfg_blocks c n2;
      bl c (program_counter (n1,0) ** continuing ** stack_height h ** gas_pred a ** blocktype ty1 **  rest) co1 q;
      bl c (program_counter (n2,0) ** continuing ** stack_height h ** gas_pred a ** blocktype ty2 **  rest) co2 q \<rbrakk>
  \<Longrightarrow> bl c (program_counter (i,j) ** continuing ** stack_height (Suc(Suc h)) ** gas_pred a ** blocktype Jumpi ** stack (Suc h) x ** stack h y ** rest) [] q"
| bl_weaken_pre:
 "bl c p i q \<Longrightarrow> (\<And>s. r s \<Longrightarrow> p s) \<Longrightarrow> bl c r i q"


(* Example *)
definition c where
"c = build_cfg [Stack (PUSH_N [1]), Stack (PUSH_N [0x6]), Pc JUMP, Misc STOP, Pc JUMPDEST, Misc STOP]"

lemmas evm_fun_simps = inst_stack_numbers.simps stack_inst_code.simps inst_size_def inst_code.simps 
pc_inst_numbers.simps 
misc_inst_numbers.simps

schematic_goal c_val:
 " c = ?p"
apply(simp add: c_def build_cfg_def word_rcat_def bin_rcat_def Let_def 
evm_fun_simps
update_edges_def extract_indexes_def
  byteListInt_def find_block_def deconstruct_def split:if_splits nat.splits option.splits)
done

lemma " bl c
(stack_height 0 ** program_counter (0,0) ** gas_pred 1000 ** continuing ** blocktype Jump)
(snd (the (cfg_blocks c 0)))
(stack 0 (word_rcat [1::byte]) ** program_counter (6,1))
"
apply(unfold c_val )
apply(simp add: snd_def )
apply(rule in_block)
 apply (rule  inst_rule_weaken_pre[OF push_n[where h=0 and g=1000 and rest=emp and t=Jump ]])
 apply simp
apply(rule in_block)
apply (rule  inst_rule_weaken_pre[OF push_n[where h=1 and g="1000-Gverylow" 
      and rest="stack 0 (word_rcat [1])" and t=Jump]])
 apply clarsimp
 apply (rule conjI, assumption)
 apply assumption
apply (rule bl_weaken_pre[OF jump_transition[where i=0 and j="2 + 1 + int (length [6::8 word])" 
      and h=1 and a="1000 - Gverylow - Gverylow" and x="(word_rcat [6::byte])" and rest="stack 0 (word_rcat [1::byte])"]])
 apply simp
 apply simp
 apply(simp)
  apply(auto)
apply(rule in_block)
apply (rule inst_rule_weaken_pre[OF jumpdest[where h="Suc 0" and g="(1000 - 2 * Gverylow)"
      and rest="stack 0 (word_rcat [1]) ** blocktype No"]])
 apply(simp)
 apply(rule conjI)
 apply(auto)
apply(rule in_block)
apply (rule inst_rule_weaken_pre[OF stop[where h="Suc 0" 
      and rest="blocktype No ** stack 0 (word_rcat [1]) ** gas_pred (1000 - 2 * Gverylow - Gjumpdest)"]])
 apply(simp)
 apply(rule conjI)
 apply auto
apply (rule bl_weaken_pre[OF empty[where rest="not_continuing ** action (ContractReturn []) 
        ** stack_height (Suc 0) ** gas_pred (1000 - 2 * Gverylow - Gjumpdest)"]])
 apply(simp)
done

end
