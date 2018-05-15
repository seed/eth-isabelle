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

lemma program_sem_not_incrase:
 "program_sem_t (g_cctx g) net (InstructionContinue x) =  InstructionToEnvironment act ctx x23 \<Longrightarrow>
 get_vctx_gas y = vctx_gas ctx \<Longrightarrow>
 get_vctx_gas y \<le> get_vctx_gas g"
sorry

lemma global_step_not_increase_gas:
  "\<not> get_vctx_gas g \<le> 0
   \<Longrightarrow> Continue g'= global_step net (g\<lparr>g_vmstate := InstructionContinue x\<rparr>)
   \<Longrightarrow> (get_vctx_gas g') \<le> (get_vctx_gas g)"
 apply (clarsimp simp: global_step_def)
 apply (cases "program_sem_t (g_cctx g) net (InstructionContinue x)"; clarsimp)
  using program_sem_t_not_continue apply fastforce
 apply (clarsimp simp: envstep_def Let_def split: contract_action.splits if_splits)
  apply (erule program_sem_not_incrase)
sorry

termination global_sem
  apply (relation "((\<lambda>(net,gs). nat (get_vctx_gas gs))) <*mlex*>
                   (measure (\<lambda>(net,gs). get_callstack_length gs))")
  apply (rule wf_mlex)
  apply simp

  apply (clarsimp simp: mlex_eq)
  apply (frule (1) global_step_not_increase_gas)
  apply (drule mp)
   apply (erule nat_mono)
  apply (rule ccontr)
  apply (erule notE) back
  apply simp

oops
end