theory EvmCode
imports "Evm"
  "Block"
begin
definition uint4 :: "4 word \<Rightarrow> int "
  where "uint4 \<equiv> uint"

definition uint8 :: "8 word \<Rightarrow> int "
  where "uint8 \<equiv> uint"

definition uint160 :: "160 word \<Rightarrow> int "
  where "uint160 \<equiv> uint"

definition uint256 :: "256 word \<Rightarrow> int "
  where "uint256 \<equiv> uint"
    
export_code program_sem empty_program empty_storage uint4 uint8 uint256 uint160 network_of_block_number 
start_transaction end_transaction next0
in OCaml
module_name EvmCodeModule file "evmCode2.ml"


end