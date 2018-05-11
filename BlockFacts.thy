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

lemma
  "program_sem_t (g_cctx g) net (InstructionContinue ir) = InstructionContinue ir'
  \<Longrightarrow> \<not> vctx_gas ir \<le> 0
  \<Longrightarrow> vctx_gas ir' < vctx_gas ir"
  apply (induct arbitrary: ir rule: program_sem_t.induct)
  apply clarsimp
  apply (simp add: program_sem_t.simps split: option.splits )
    apply (clarsimp simp add: check_resources_def prod.case_eq_if  split:if_splits)
(* apply (drule (3) inst_sem_cont_gas_decrease)
 apply (frule (1) inst_sem_gas_consume)
  apply (clarsimp simp: vctx_next_instruction_def inst_stack_numbers.simps split: option.splits)
using instruction_sem_not_continuing[where inst="Misc STOP", simplified]
  apply (simp add: program_sem_t.simps)
*)
oops

termination global_sem
  apply (relation "measure (\<lambda>(net,gs). nat
  (case gs of
     Continue g \<Rightarrow> 
      (case g_vmstate g of
     InstructionContinue vctx \<Rightarrow>  vctx_gas vctx
   | InstructionToEnvironment act vctx _ \<Rightarrow> vctx_gas vctx)
  | Unimplemented \<Rightarrow> 0
  | Finished _\<Rightarrow> 0))")
   apply (simp)
  apply (simp only: measure_def inv_image_def split_def)
  apply (clarsimp simp: get_vctx_gas.simps split:instruction_result.splits)
  apply (rule conjI, clarsimp simp: get_vctx_gas.simps)+
defer

  apply (clarsimp simp: get_vctx_gas.simps split:instruction_result.splits)
defer
  apply (clarsimp simp: get_vctx_gas.simps split:instruction_result.splits)
  apply (rule conjI, clarsimp simp: get_vctx_gas.simps)+
defer  
  apply (clarsimp simp: get_vctx_gas.simps split:instruction_result.splits)
  apply (simp add: bigstep_def Let_def)
  apply(case_tac "g_vmstate gs "; clarsimp)
  subgoal for const net var inst
    apply (case_tac "instruction_sem var const inst net" ; simp)
    apply (clarsimp simp add: check_resources_def prod.case_eq_if )
     apply (frule instruction_sem_continuing)
     apply (erule (2) inst_sem_gas_consume)
  apply (simp add: vctx_next_instruction_def split:option.splits)
    done
done
    
l
end