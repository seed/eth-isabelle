open EvmCode
open Word256
open Word160
open Word8
open Word4
open Conv

let empty_program : unit program_ext =
        Program_ext ((fun _ -> None), (Int_of_integer (Big_int.big_int_of_int 0)), ());;

type address = word160 
let dummy_constant_ctx : unit constant_ctx_ext =
        Constant_ctx_ext (empty_program, w160_from_int 0, (fun _ -> true), ());;


let empty_memory = (fun _ -> 
        word_of_int (len0_bit0 (len0_bit0 (len0_bit0 (len0_num1)))) zero_int);;

let empty_balance _ = w256_from_int 0

let dummy_address = w160_from_int 144545

let empty_ext_program _ = empty_program

let dummy_block_info : unit block_info_ext =
  Block_info_ext ((fun _ -> w256_from_int 0),
  dummy_address,
  w256_from_int 0,
  w256_from_int 0,
  w256_from_int 0,
  w256_from_int 0,
  ())
(*
let empty_memory : memory = (fun _ -> byte_of_big_int Big_int.zero_big_int)

let zero_word = word256_of_big_int Big_int.zero_big_int

let empty_balance _ = zero_word

let dummy_address = word160_of_big_int (Big_int.big_int_of_int 144545)

let empty_ext_program (_ : Word160.word160) = empty_program

let dummy_block_info : block_info =
  { block_blockhash = (fun _ -> zero_word)
  ; block_coinbase = dummy_address
  ; block_timestamp = zero_word
  ; block_number = zero_word
  ; block_difficulty = zero_word
  ; block_gaslimit = zero_word
  }
*)

let dummy_variable_con : unit variable_ctx_ext =
  Variable_ctx_ext ([],
  empty_memory,
  Int_of_integer (Big_int.big_int_of_int 0),
  empty_storage,
  Int_of_integer (Big_int.big_int_of_int 0),
  empty_balance,
  dummy_address,
  w256_from_int 0,
  [],
  empty_storage,
  empty_balance,
  dummy_address,
  empty_ext_program,
  dummy_block_info,
  Int_of_integer (Big_int.big_int_of_int 50000),
  (fun _ -> false),
  [],
  [], 
  Int_of_integer (Big_int.big_int_of_int 0),
  w256_from_int 21000000000,
  ())

(*
let dummy_variable_con : variable_ctx =
  { vctx_stack = []
  ; vctx_memory = empty_memory
  ; vctx_memory_usage = Nat_big_num.of_int 0
  ; vctx_storage = empty_storage
  ; vctx_pc = Nat_big_num.of_int 0
  ; vctx_balance = empty_balance
  ; vctx_caller = dummy_address
  ; vctx_value_sent = zero_word
  ; vctx_data_sent = []
  ; vctx_storage_at_call = empty_storage
  ; vctx_balance_at_call = empty_balance
  ; vctx_origin = dummy_address
  ; vctx_gasprice = Conv.word256_of_big_int (Big_int.big_int_of_int 21000000000)
  ; vctx_ext_program = empty_ext_program
  ; vctx_block = dummy_block_info
  ; vctx_gas = Nat_big_num.of_int 50000
  ; vctx_account_existence = (fun _ -> false)
  ; vctx_touched_storage_index = []
  ; vctx_logs = []
  ; vctx_refund = Nat_big_num.of_int 0
  }
*)
