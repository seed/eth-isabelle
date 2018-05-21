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

lemma env_step_keep_pos:
 "\<not> get_vctx_gas g \<le> 0
 \<Longrightarrow> Continue g' = envstep net g
 \<Longrightarrow> \<not> get_vctx_gas g' \<le> 0"
oops 


lemma program_sem_t_not_continue:
   " program_sem_t const net ir \<noteq> InstructionContinue z"
 using program_sem_t_in_program_sem[where const=const and net=net and ir=ir]
  apply clarsimp
  apply (drule_tac x=k in spec)+
  apply clarsimp
  apply (drule_tac x=z in spec)+
  apply clarsimp
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
  apply (drule_tac x="x1" in spec)
  apply (simp add: program_sem.simps next_state_def)
  apply (simp add: instruction_sem_def)
  apply (case_tac inst ;clarsimp)
  apply (clarsimp simp add: instruction_failure_result_def)
 apply (clarsimp simp add:  subtract_gas.simps meter_gas_ge_0)
  apply (case_tac x2 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def  stack_2_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 split: list.splits)+
  apply (case_tac x3 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def  stack_2_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 split: list.splits)+
  apply (case_tac x4 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def split: list.splits if_splits)+
  apply (case_tac x5 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def   stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def split: option.splits list.splits if_splits)+
  apply (case_tac x7 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def   stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def split: option.splits list.splits if_splits)+
  apply (case_tac x8 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def   stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def split: option.splits list.splits if_splits)+
  apply (case_tac x9 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def   stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def split: option.splits list.splits if_splits)+
  apply (case_tac x2 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def   stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def split: option.splits list.splits if_splits)+
  apply (case_tac x9 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def   stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def jumpi_def strict_if_def blockedInstructionContinue_def blocked_jump_def  split: option.splits list.splits if_splits)+
  apply (case_tac x2 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def   stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def jumpi_def strict_if_def blockedInstructionContinue_def blocked_jump_def  split: option.splits list.splits if_splits)+
  apply (case_tac x9 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def stack_0_0_op_def  stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def jumpi_def strict_if_def blockedInstructionContinue_def blocked_jump_def  pc_def split: option.splits list.splits if_splits)+
  apply (case_tac x10 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def stack_0_0_op_def  stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def jumpi_def strict_if_def blockedInstructionContinue_def blocked_jump_def  pc_def pop_def swap_def log_def delegatecall_def split: option.splits list.splits if_splits)+
  apply (case_tac x12 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def stack_0_0_op_def  stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def jumpi_def strict_if_def blockedInstructionContinue_def blocked_jump_def  pc_def pop_def swap_def log_def delegatecall_def ret_def split: option.splits list.splits if_splits)+
  apply (case_tac x13 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def stack_0_0_op_def  stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def jumpi_def strict_if_def blockedInstructionContinue_def blocked_jump_def  pc_def pop_def swap_def log_def delegatecall_def ret_def stop_def create_def vctx_advance_pc_def call_def callcode_def suicide_def split: option.splits list.splits if_splits)+
  apply (case_tac x13 ; clarsimp)
 apply (clarsimp simp add: instruction_failure_result_def stack_0_0_op_def  stack_0_1_op_def  stack_2_1_op_def stack_3_1_op_def  stack_1_1_op_def subtract_gas.simps meter_gas_ge_0 sha3_def Let_def general_dup_def mload_def mstore_def mstore8_def calldatacopy_def codecopy_def extcodecopy_def  sstore_def jump_def jumpi_def strict_if_def blockedInstructionContinue_def blocked_jump_def  pc_def pop_def swap_def log_def delegatecall_def ret_def stop_def create_def vctx_advance_pc_def call_def callcode_def suicide_def split: option.splits list.splits if_splits)+
done  

lemma program_sem_not_increase:
 " instr_gas (program_sem (\<lambda>_. ()) (g_cctx g) k net x) \<le> instr_gas x"
  apply(induct k arbitrary: x)
   apply(simp add: program_sem.simps)+
  apply(simp add: next_state_def)
  apply(case_tac x; clarsimp)
   apply(case_tac "vctx_next_instruction x1 (g_cctx g)"; clarsimp)
    apply(subst program_sem_ItoE)
    apply simp
   apply(rule conjI, clarsimp)+
  apply(drule meta_spec, erule order_trans)
prefer 4
 apply (simp add: program_sem_ItoE)
prefer 3
  apply clarsimp
 apply (simp add: program_sem_ItoE)
  apply (drule_tac x="instruction_sem x1 (g_cctx g) a net" in meta_spec)
  apply clarsimp
  apply (erule order_trans)
prefer 3
  apply clarsimp
 apply (simp add: program_sem_ItoE)
 apply (clarsimp simp: check_resources_def)
 apply (erule (2) instr_gas_le_vctx_gas)
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
  apply (cut_tac k=k in program_sem_not_increase[where g="g" and net=net and x="(InstructionContinue x)"])
   apply simp
  apply (drule sym[where s="InstructionToEnvironment _ _ _"])
  apply simp
done

lemma global_step_not_increase_gas:
  "\<not> get_vctx_gas g \<le> 0 \<Longrightarrow>
vctx_gas x \<le> get_vctx_gas g \<Longrightarrow>
   Continue g'= global_step net (g\<lparr>g_vmstate := InstructionContinue x\<rparr>)
   \<Longrightarrow> (get_vctx_gas g') \<le> (get_vctx_gas g)"
 apply (clarsimp simp: global_step_def)
 apply (cases "program_sem_t (g_cctx g) net (InstructionContinue x)"; clarsimp)
  using program_sem_t_not_continue apply fastforce
 apply (clarsimp simp: envstep_def Let_def split: contract_action.splits if_splits)
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