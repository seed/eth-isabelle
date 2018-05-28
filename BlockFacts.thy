(*
   Copyright 2017 Sidney Amani
   Copyright 2017 Maksym Bortin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
theory BlockFacts
  imports EvmFacts
          "lem/Block"
         "attic/Apply_Trace_Cmd"
begin


lemma env_step_instruction_continue:
 "Continue g' = envstep net g
  \<Longrightarrow> \<exists>x. g_vmstate g' = InstructionContinue x"
  apply (clarsimp simp: envstep_def Let_def)
  apply (cases "g_vmstate g" ; clarsimp)
  apply (rename_tac action x y)
  apply (case_tac action ; clarsimp simp : Let_def split: if_split_asm list.splits stack_hint.splits)
done


subsection "More on program-sem"

lemma program_sem_t_not_continue:
   " program_sem_t const net ir \<noteq> InstructionContinue z"
 using program_sem_t_in_program_sem[where const=const and net=net and ir=ir]
  apply clarsimp
  apply (drule_tac x=k in spec)+
  apply clarsimp
  apply (drule_tac x=z in spec)+
  apply clarsimp
  done


lemma program_sem_last_step_split :
 "program_sem st gc k net (InstructionContinue x) = InstructionToEnvironment z vctx retv \<Longrightarrow>
  \<exists>x' k'. k' \<le> k \<and> program_sem st gc k' net (InstructionContinue x) = InstructionContinue x' \<and>
          next_state st gc net (InstructionContinue x') = InstructionToEnvironment z vctx retv"
  apply (induct k arbitrary: x)
   apply(simp add: program_sem.simps)+
  apply(case_tac "next_state st gc net (InstructionContinue x)")
   apply clarsimp
   apply(drule meta_spec, drule meta_mp, assumption)
   apply clarsimp
   apply(rule_tac x=x' in exI)
   apply(rule_tac x="Suc k'" in exI, simp add: program_sem.simps)
  apply clarsimp
  apply(rule_tac x=x in exI)
  apply(rule_tac x=0 in exI, simp add: program_sem_ItoE)
  apply(simp add: program_sem.simps)
  done



fun instr_gas
  where "instr_gas (InstructionContinue x) = vctx_gas x" |
        "instr_gas (InstructionToEnvironment a ctx b) = vctx_gas ctx"

lemma Cextra_ge_0:
  "0 \<le> Cextra  a b c"
  by (simp add:  gas_simps)

lemma Ccall_ge_0:
  "0 \<le> Ccall s0 s1 s2 recipient_empty
            remaining_gas net mmu_extra"
  unfolding Ccall_def
  using Cextra_ge_0 Cgascap_gt_0
  by (clarsimp simp add: ordered_comm_monoid_add_class.add_nonneg_pos)+

lemma Csuicide_ge_0:
  "0 \<le> Csuicide recipient_empty net"
  unfolding Csuicide_def
  by (auto split: if_splits
           simp add: gas_simps Gsuicide_def)

lemma thirdComponentOfC_ge_0:
  "0 \<le> thirdComponentOfC  i s0 s1 s2 s3 recipient_empty orig_val new_val remaining_gas net mmu_extra"
  unfolding thirdComponentOfC_def
  apply (case_tac i ; simp add: gas_simps del: Cextra_def )
           apply (case_tac x2; simp add: gas_simps)
          apply (case_tac x3; simp add: gas_simps )
         apply (case_tac x4 ; simp add: gas_simps)
         using log256floor_ge_0[where s="uint s1"]
                 apply (simp add: )
              apply (clarsimp; simp add: word_less_def word_neq_0_conv)
                apply (case_tac x5; simp add: gas_simps)
              apply (case_tac x7; simp add: gas_simps)
                apply (case_tac "s2 = 0" ; auto simp: word_less_def word_neq_0_conv)
                apply (case_tac "s2 = 0" ; auto simp: word_less_def word_neq_0_conv)
              apply (case_tac "s3 = 0" ; auto simp: word_less_def word_neq_0_conv)
            apply (case_tac x8; simp add: gas_simps Csstore_def)
            apply (case_tac x9; simp add: gas_simps Csstore_def)
           apply (case_tac x10; simp add: gas_simps Csstore_def)
          apply ( case_tac x12; case_tac "s1 = 0"; 
             simp add: gas_simps word_less_def word_neq_0_conv)
         apply (clarsimp split: misc_inst.splits)
         apply (rule conjI, clarsimp simp add: gas_simps L_def)
         apply (clarsimp simp: Csuicide_ge_0 Ccall_ge_0 Gzero_def  Gcreate_def  split:if_splits)
         done

lemma meter_gas_ge_0:
 " 0 \<le> meter_gas inst var const net"
  using Cmem_lift[OF
    vctx_memory_usage_never_decreases[where i=inst and v=var]]
  apply (clarsimp simp add: C_def meter_gas_def Cmem_def Gmemory_def Let_def)
  apply(case_tac inst)
apply( simp add: new_memory_consumption.simps vctx_next_instruction_default_def vctx_next_instruction_def max_def;   fastforce  intro: ordered_comm_monoid_add_class.add_nonneg_pos 
          thirdComponentOfC_ge_0)+
  apply (rename_tac x)
 apply (case_tac x)
 apply (simp add: max_def new_memory_consumption.simps vctx_next_instruction_default_def thirdComponentOfC_ge_0  split: if_splits)+
done

lemmas inst_sem_simps =
  instruction_failure_result_def stack_0_0_op_def  stack_0_1_op_def
  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps 
  meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def
  mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def
  jump_def jumpi_def strict_if_def blockedInstructionContinue_def
  blocked_jump_def  pc_def pop_def swap_def log_def delegatecall_def
  ret_def stop_def create_def vctx_advance_pc_def call_def callcode_def
  suicide_def

lemma instr_gas_le_vctx_gas:
 "vctx_next_instruction ctx c = Some inst
  \<Longrightarrow> meter_gas inst ctx c net \<le> vctx_gas ctx
  \<Longrightarrow> check_resources ctx c (vctx_stack ctx) inst net
  \<Longrightarrow> instr_gas (instruction_sem ctx c inst net) \<le> vctx_gas ctx"
 apply (case_tac "instruction_sem ctx c inst net" ;clarsimp)
 apply (case_tac "\<not> vctx_gas ctx \<le> 0")
 apply (drule (3) inst_sem_cont_gas_decrease)
  apply simp
 apply simp
 apply (drule program_sem_no_gas_not_continuing'[where c=c and net=net])
  apply (drule_tac x=1 in spec)
  apply simp
  apply (rename_tac x)
  apply (drule_tac x="x" in spec)
  apply (simp add: program_sem.simps next_state_def)
  apply (simp add: instruction_sem_def)
  apply (case_tac inst ;clarsimp)
  apply ((rename_tac x, case_tac x; clarsimp),
         (clarsimp simp: inst_sem_simps
                   split: option.splits list.splits if_splits)+)+
done


lemma program_sem_not_increase:
 "instr_gas (program_sem z gcctx k net x) \<le> instr_gas x"
  apply(induct k arbitrary: x)
   apply(simp add: program_sem.simps)+
  apply(simp add: next_state_def)
  apply(case_tac x; clarsimp)
  apply (rename_tac x)
   apply(case_tac "vctx_next_instruction x gcctx"; clarsimp)
    apply(subst program_sem_ItoE)
    apply simp
   apply(rule conjI, clarsimp)+
  apply(drule meta_spec, erule order_trans)
  apply clarsimp
 apply (erule (2) instr_gas_le_vctx_gas)
 apply (simp add: program_sem_ItoE check_resources_def)+
done



lemma program_sem_t_not_increase:
 "program_sem_t (g_cctx g) net (InstructionContinue x) = InstructionToEnvironment act ctx x23 \<Longrightarrow>
  vctx_gas x \<le> get_vctx_gas g \<Longrightarrow>
 get_vctx_gas y = vctx_gas ctx \<Longrightarrow>
 get_vctx_gas y \<le> get_vctx_gas g"
 using program_sem_t_in_program_sem[where const="g_cctx g" and net=net and ir="(InstructionContinue x)"]
 apply clarsimp
  apply (drule_tac x=k in spec)+
  apply clarsimp
  apply (cut_tac k=k in program_sem_not_increase[where z="(\<lambda>_. ())" and  gcctx="g_cctx g" and net=net and x="(InstructionContinue x)"])
   apply simp
  apply (drule sym[where s="InstructionToEnvironment _ _ _"])
  apply simp
done                                        




lemma program_sem_t_ContractCall_gas :
 "program_sem_t gc net (InstructionContinue x) = InstructionToEnvironment (ContractCall callarg) vctx retv \<Longrightarrow>
 vctx_gas vctx + uint (callarg_gas callarg) \<le> vctx_gas x"
  apply(insert program_sem_t_in_program_sem[where const=gc and net=net and ir="InstructionContinue x"])
  apply clarsimp
  apply(drule sym, drule program_sem_last_step_split)
  apply clarify
  apply(cut_tac z="(\<lambda>_. ())" and gcctx=gc and k=k' and net=net and x="InstructionContinue x" in program_sem_not_increase)
  apply simp
  apply(erule order_trans[rotated -1])


(*
 apply (induct arbitrary: x rule: program_sem_t.induct)
  apply clarsimp
  apply (drule_tac x=x in meta_spec)
  apply clarsimp
  apply (simp add: instruction_sem_def)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps split:option.splits if_splits)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps split:option.splits if_splits list.splits)
  apply (simp add: meter_gas_def Let_def C_def thirdComponentOfC_def Ccall_def )
  apply (fold calc_memu_extra_def)
  apply (simp add: max_def)
  apply (rule conjI, clarsimp)+
 apply (clarsimp simp: Ccallgas_def)
  apply (rule conjI, clarsimp)+
*)
sorry

lemma next_state_ItoE:
 "next_state st gc net (InstructionToEnvironment act vctx retv) = InstructionToEnvironment act vctx retv "
  by (simp add: next_state_def)

lemma next_state_call:
 "next_state st gc net (InstructionContinue x) =  InstructionToEnvironment (ContractCall callarg) vctx retv
  \<Longrightarrow>
 vctx_next_instruction x gc \<in> Some ` { (Misc CALL), (Misc CALLCODE)}"
 apply (clarsimp simp: next_state_def)
  apply (clarsimp split: option.splits if_splits)
  apply (clarsimp simp: instruction_sem_def)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps split:option.splits if_splits list.splits)+
done

lemma program_sem_last_step:
 "program_sem st gc k net (InstructionContinue x) = InstructionToEnvironment (ContractCall callarg) vctx retv
 \<Longrightarrow> \<exists>x'. k>0
  \<and> ((1 < k \<and> program_sem st gc (k - 1) net (InstructionContinue x) = InstructionContinue x') \<or> (k = 1 \<and> x=x') )
  \<and> program_sem st gc 1 net (InstructionContinue x') = InstructionToEnvironment (ContractCall callarg) vctx retv"
 apply (induct k arbitrary: x)
  apply (simp add: program_sem.simps)
  apply (simp add: program_sem.simps)

  apply (cut_tac ?pr1.0="(InstructionContinue x)" in next_state_def[where stopper=st and c=gc and net=net]) 
  apply simp
 apply (thin_tac "next_state st gc net (InstructionContinue x) = _")
   apply (clarsimp split: option.splits simp add: program_sem_ItoE)
   apply (clarsimp split: if_splits simp: program_sem_ItoE)
find_theorems name:program_sem 

(*    apply (case_tac k; clarsimp)
   apply (simp add: program_sem.simps)
   apply (simp add: next_state_def)
   apply (rule_tac x=x in exI)
   apply clarsimp
  *)
 apply (clarsimp simp: instruction_sem_def)
 


    apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps   program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (drule meta_spec, drule (1) meta_mp)
  apply (clarsimp)
apply(erule disjE)
  apply (rule_tac x=x' in exI)
  apply clarsimp
  apply (case_tac k; clarsimp)
apply(simp (no_asm) add: program_sem.simps next_state_def)
 apply(simp split: option.splits add: instruction_sem_def inst_sem_simps)
apply clarsimp
apply(rule_tac x="(x\<lparr>vctx_pc := vctx_pc x + inst_size (Bits inst_AND), vctx_stack := [x21 AND x21a],
               vctx_gas := vctx_gas x - meter_gas (Bits inst_AND) x gc net,
               vctx_memory_usage :=
                 new_memory_consumption (Bits inst_AND) (vctx_memory_usage x) (vctx_stack_default 0 x)
                  (vctx_stack_default 1 x) (vctx_stack_default 2 x) (vctx_stack_default 3 x)
                  (vctx_stack_default 4 x) (vctx_stack_default 5 x) (vctx_stack_default 6 x)\<rparr>)" in exI)
apply simp
apply(simp (no_asm) add: program_sem.simps next_state_def)
 apply(simp split: option.splits add: instruction_sem_def inst_sem_simps)

prefer 17
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)




  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)

  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (rename_tac x', case_tac x'; clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+

  apply (clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
  apply (rule exI, erule conjI[rotated],
       simp (no_asm) add: next_state_def, simp add: instruction_sem_def inst_sem_simps)+
  apply (clarsimp simp: inst_sem_simps program_sem.simps next_state_ItoE
      program_sem_ItoE split:option.splits if_splits list.splits)
oops

lemma global_step_not_increase_gas:
  "\<not> get_vctx_gas g \<le> 0 \<Longrightarrow>
   g_vmstate g = InstructionContinue x \<Longrightarrow>
vctx_gas x \<le> get_vctx_gas g \<Longrightarrow>
   Continue g'= global_step net (g\<lparr>g_vmstate := InstructionContinue x\<rparr>)
   \<Longrightarrow> (get_vctx_gas g') \<le> (get_vctx_gas g)"
 apply (clarsimp simp: global_step_def)
 apply (cases "program_sem_t (g_cctx g) net (InstructionContinue x)"; clarsimp)
  using program_sem_t_not_continue apply fastforce
 apply (clarsimp simp: envstep_def Let_def split: contract_action.splits if_splits)
  apply (simp add: get_vctx_gas_def)
  apply (erule xxx)
  apply (rename_tac  vctx retv callarg)
  apply (erule program_sem_t_not_increase)
  apply (simp add: get_vctx_gas_def)
  apply (case_tac "g_vmstate g" ;clarsimp)
sorry

termination global_sem
  apply (relation "((\<lambda>(net,gs). nat (get_vctx_gas gs))) <*mlex*>
                   (measure (\<lambda>(net,gs). get_callstack_length gs))")
  apply (rule wf_mlex)
  apply simp

  apply (clarsimp simp: mlex_eq)
  apply (frule_tac x=x1 in global_step_not_increase_gas)
apply(simp add: get_vctx_gas_def)
apply assumption
  apply (drule mp)
   apply (erule nat_mono)
  apply (rule ccontr)
  apply (erule notE) back
  apply simp

oops
end