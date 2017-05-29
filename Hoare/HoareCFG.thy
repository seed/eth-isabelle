theory "HoareCFG"

imports "HoareTripleForInstructions"

begin
type_synonym position = "int * int"
type_synonym pred = "(position state_element set \<Rightarrow> bool)"

(* To be completed *)
inductive triple_inst :: "pred \<Rightarrow> pos_inst \<Rightarrow> pred \<Rightarrow> bool" where
  inst_push_n : "triple_inst (\<langle> h \<le> 1023 \<and> length lst > 0 \<and> 32 \<ge> length lst\<rangle> **
                       continuing **
                       program_counter (n,m) **
                       stack_height h **
                       gas_pred g **
                       rest
                      )
                      ((n,m), Stack (PUSH_N lst))
                      (continuing **
                       program_counter (n,m + 1 + int (length lst)) **
                       stack_height (Suc h) **
                       gas_pred (g - Gverylow) **
                       stack h (word_rcat lst) **
                       rest
                      )"
| inst_stop : "triple_inst 
          (\<langle> h \<le> 1024 \<rangle> ** continuing ** program_counter k ** stack_height h ** rest)
          (k, Misc STOP)
          (stack_height h ** not_continuing ** program_counter k ** action (ContractReturn []) ** rest )"
| inst_jumpdest : "triple_inst (\<langle> h \<le> 1024 \<rangle> **
                       continuing ** 
                       program_counter (n,m) **
                       stack_height h **
                       gas_pred g **
                       rest
                      )
                      (_, Pc JUMPDEST)
                      (continuing **
                       program_counter (n,m + 1) **
                       stack_height h **
                       gas_pred (g - Gjumpdest) **
                       rest
                      )"
| inst_strengthen_pre: "triple_inst p i q \<Longrightarrow> (\<And>s. r s \<longrightarrow> p s) \<Longrightarrow> triple_inst r i q"
| inst_false_pre: "triple_inst \<langle>False\<rangle> i post"

inductive triple_seq :: "pred \<Rightarrow> vertex \<Rightarrow> pred \<Rightarrow> bool" where
  seq_inst : "
  \<lbrakk> triple_inst pre x q;
    triple_seq q (n, xs, ty) post \<rbrakk>
  \<Longrightarrow> triple_seq pre (n, x#xs, ty) post"
| seq_no : "triple_seq (p ** rest) (n, [], No) p"
| seq_next : "triple_seq pre (n, [], Next) pre"
| seq_jump : "(\<And>s. pre s \<longrightarrow> post s) \<Longrightarrow>
   triple_seq pre (n, [], Jump) post"
| seq_jumpi : "
   (\<And>s. pre s \<longrightarrow> post s) \<Longrightarrow>
   triple_seq pre (n, [], Jumpi) post"
| seq_weaken_post : "triple_seq pre a post \<Longrightarrow> (\<And>s. post s \<longrightarrow> q s) \<Longrightarrow> triple_seq pre a q "
| seq_strengthen_pre: "triple_seq p i q \<Longrightarrow> (\<And>s. r s \<longrightarrow> p s) \<Longrightarrow> triple_seq r i q" 
| seq_false_pre: "triple_seq \<langle>False\<rangle> i post"

inductive triple_cfg :: "cfg \<Rightarrow> pred \<Rightarrow> vertex \<Rightarrow> pred \<Rightarrow> bool" where
  cfg_no : " triple_seq pre (n, insts, No) post
  \<Longrightarrow> triple_cfg cfg pre (n, insts, No) post"
| cfg_next : " 
  \<lbrakk> cfg_edges cfg n = Some (i,_);
    cfg_blocks cfg i = Some block;
    triple_seq pre (n, insts, Next) (program_counter (n,m) ** q);
    triple_cfg cfg (program_counter (i,0) ** q) block post\<rbrakk>
  \<Longrightarrow> triple_cfg cfg pre (n, insts, Next) post"
| cfg_jump : " 
  \<lbrakk> cfg_edges cfg n = Some (i,_);
    cfg_blocks cfg i = Some block;
    triple_seq pre (n, insts, Jump) 
      (\<langle> h \<le> 1023 \<rangle> **
       stack_height (Suc h) **
       stack h _ **
       gas_pred g **
       continuing **
       program_counter (n,m) **
       rest
      );
    triple_cfg cfg 
      (stack_height h **
       gas_pred (g - Gmid) **
       continuing **
       program_counter (i,0) **
       rest
      ) block post\<rbrakk>
  \<Longrightarrow> triple_cfg cfg pre (n, insts, Jump) post"
| cfg_jumpi : " 
  \<lbrakk> cfg_edges cfg n = Some (i, Some j);
    cfg_blocks cfg i = Some blocki;
    cfg_blocks cfg j = Some blockj;
    triple_seq pre (n, insts, Jumpi) 
        ((\<langle> h \<le> 1022  \<rangle> **
         stack_height (Suc (Suc h)) **
         stack (Suc h) d **
         stack h cond **
         gas_pred g **
         continuing **
         program_counter (n,m) **
          rest
        ));
    r = (stack_height h **
         gas_pred (g - Ghigh) **
         continuing **
          rest
        );
    (cond = 0 \<Longrightarrow> triple_cfg cfg (r ** program_counter (i,0)) blocki post);
    (cond \<noteq> 0 \<Longrightarrow> triple_cfg cfg (r ** program_counter (j,0)) blockj post)
\<rbrakk>
  \<Longrightarrow> triple_cfg cfg pre (n, insts, Jumpi) post"
| cfg_false_pre: "triple_cfg cfg \<langle>False\<rangle> i post"

definition triple_inst_sem :: "pred \<Rightarrow> pos_inst \<Rightarrow> pred \<Rightarrow> bool" where
"triple_inst_sem pre inst post ==
    \<forall> co_ctx presult rest stopper. no_assertion co_ctx \<longrightarrow>
       (pre ** code {inst} ** rest) (instruction_result_as_set co_ctx presult) \<longrightarrow>
       ((post ** code {inst} ** rest) (instruction_result_as_set co_ctx (program_sem stopper co_ctx 1 presult)))"

lemma strengthen_pre_inst_sem:
  assumes  "triple_inst_sem P c Q"
  and      "(\<forall> s. R s \<longrightarrow> P s)"
  shows    " triple_inst_sem R c Q"
  using assms(1)
  apply (simp add: triple_inst_sem_def)
  apply(clarify)
  apply(drule_tac x = co_ctx in spec)
  apply(simp)
  apply(drule_tac x = presult in spec)
  apply(drule_tac x = rest in spec)
  apply simp
  apply (erule impE)
   apply (sep_drule assms(2)[rule_format])
   apply assumption
  apply simp
done

end