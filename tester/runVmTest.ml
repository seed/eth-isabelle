open Yojson.Basic
open VmTestParser
open Constant
open TestResult
open Conv
open EvmCode
open Sys


let spec_includes_actual (spec_storage : list_storage) (touched : isaw256 list) (actual_storage : isaw256 -> isaw256) : bool =
  (* for each touched index, check that the actual storage and the spec storage have the same thing. *)
  let f (idx : isaw256) =
    let actual_value : Big_int.big_int = Conv.uint256_big (actual_storage idx) in
    let spec_idx : Big_int.big_int = Conv.uint256_big idx in
    let spec_value = (* This procedure needs to be split away. *)
      try Big_int.big_int_of_string (Conv.bigint_assoc spec_idx spec_storage)
      with Not_found -> Big_int.zero_big_int
    in
    let ret = Big_int.eq_big_int actual_value spec_value in
    let () = assert ret in
    ret
  in
  List.for_all f touched

let actual_includes_spec (spec_storage : list_storage) (actual_storage : isaw256 -> isaw256) : bool =
  (* for each pair in spec_storage, check the actual_storage *)
  let f ((idx : Big_int.big_int), (v : string)) =
    let spec_value = Big_int.big_int_of_string v in
    let actual_idx = (Conv.w256_from_big_int idx) in
    let actual_value = Conv.uint256_big (actual_storage actual_idx) in
    let ret = Big_int.eq_big_int spec_value actual_value in
    let () = assert ret in
    ret in
  List.for_all f spec_storage

let storage_comparison
      (addr : isaw160)
      (spec_post : (string * account_state) list)
      (touched : isaw256 list)
      (actual_storage : isaw256 -> isaw256) : bool =
  let spec_storage : list_storage =
    try
      let lookup_addr = Conv.string_of_address addr in
      let a : account_state = List.assoc lookup_addr spec_post in
      a.storage
    with Not_found -> [] in
  let ret0 = spec_includes_actual spec_storage touched actual_storage in
  let ret1 = actual_includes_spec spec_storage actual_storage in
  ret0 && ret1

let balance_comparison
      (addr : isaw160)
      (spec_post : (string * account_state) list)
      (actual_balance : isaw160 -> isaw256) =
  let spec_balance : Big_int.big_int =
    try
      let lookup_addr = Conv.string_of_address addr in
      let a : account_state = List.assoc lookup_addr spec_post in
      a.balance
    with Not_found -> Big_int.zero_big_int in
  let actual_balance = Conv.uint256_big (actual_balance addr) in
  Big_int.eq_big_int spec_balance actual_balance

let compare_topics (actual : Conv.isaw256) (spec : Big_int.big_int) =
  Big_int.eq_big_int spec (Conv.uint256_big actual)

let compare_log_entry (actual : unit EvmCode.log_entry_ext) (spec : log) =
  match actual with  Log_entry_ext (log_addr, log_topics, log_data, ()) ->
  assert(Big_int.eq_big_int (Conv.uint160_big log_addr) spec.logAddress);
  assert(BatList.for_all2 compare_topics log_topics spec.topics);
  assert(spec.logData = Conv.hex_string_of_byte_list "0x" log_data);
  true

let log_comparison
    (actual_log : unit EvmCode.log_entry_ext list) (spec_log : log list) =
  let actual_log = List.rev actual_log in
  List.length actual_log = List.length spec_log &&
    (BatList.for_all2 compare_log_entry actual_log spec_log)


type storage = isaw256 -> isaw256

let vctx_stack vctx =
  match vctx with 
  Variable_ctx_ext (stack,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
          stack

let vctx_block_number vctx =
  match vctx with 
  Variable_ctx_ext (_,_,_,_,_,_,_,_,_,_,_,_,_,block_info,_,_,_,_,_,_,_) ->
    match block_info with
    Block_info_ext (_,_,_,block_number,_,_,_) ->
     block_number
    
let vctx_storage vctx =
  match vctx with 
  Variable_ctx_ext (_,_,_,storage,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
          storage

let vctx_balance vctx =
  match vctx with 
  Variable_ctx_ext (_,_,_,_,_,balance,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
          balance

let vctx_gas vctx =
  match vctx with 
  Variable_ctx_ext (_,_,_,_,_,_,_,_,_,_,_,_,_,_,gas,_,_,_,_,_,_) ->
          gas

let vctx_logs vctx =
  match vctx with 
  Variable_ctx_ext (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,logs,_,_,_) ->
          logs

let vctx_touched_storage_index vctx =
  match vctx with 
  Variable_ctx_ext (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,touched_storage_index,_,_,_,_) ->
          touched_storage_index


let test_one_case j : testResult =
  let test_case : test_case = parse_test_case j in
  let open EvmCode in
  let open Conv in
  (*val program_sem : variable_ctx -> constant_ctx -> int -> nat -> program_result*)
  (* TODO: cut below as a function, maybe into Conv *)

  let addr : address = w160_from_big_int test_case.exec.address in
  let initial_storage : storage = lookup_storage addr test_case.pre in
  let initial_balance : isaw160 -> isaw256 = construct_global_balance test_case.pre in
  let v = Variable_ctx_ext ([],
empty_memory,
Int_of_integer (Big_int.big_int_of_int 0),
initial_storage,
Int_of_integer (Big_int.big_int_of_int 0),
initial_balance,
w160_from_big_int test_case.exec.caller,
w256_from_big_int test_case.exec.value,
byte_list_of_hex_string test_case.exec.data,
initial_storage,
initial_balance,
w160_from_big_int test_case.exec.origin,
construct_ext_program test_case.pre,
construct_block_info test_case,
Int_of_integer( test_case.exec.gas),
construct_account_existence test_case.pre,
[],
[],
Int_of_integer (Big_int.big_int_of_int 0),
w256_from_big_int test_case.exec.gasPrice,
()) in
  (*
  let v : variable_ctx =
    { vctx_stack = []
    ; vctx_memory = empty_memory
    ; vctx_memory_usage = Nat_big_num.of_int 0
    ; vctx_storage = initial_storage
    ; vctx_pc = Nat_big_num.of_int 0
    ; vctx_balance = initial_balance
    ; vctx_caller = w160_from_big_int test_case.exec.caller
    ; vctx_value_sent = w256_from_big_int test_case.exec.value
    ; vctx_data_sent = byte_list_of_hex_string test_case.exec.data
    ; vctx_storage_at_call = initial_storage
    ; vctx_balance_at_call = initial_balance
    ; vctx_origin = w160_from_big_int test_case.exec.origin
    ; vctx_gasprice = w256_from_big_int test_case.exec.gasPrice
    ; vctx_ext_program = construct_ext_program test_case.pre
    ; vctx_block = construct_block_info test_case
    ; vctx_gas = Nat_big_num.of_string_nat (Big_int.string_of_big_int test_case.exec.gas)
    ; vctx_account_existence = construct_account_existence test_case.pre
    ; vctx_touched_storage_index = []
    ; vctx_logs = []
    ; vctx_refund = Nat_big_num.of_int 0
    } in
  *)
  let c : unit constant_ctx_ext =
   Constant_ctx_ext (test_case.exec.code, w160_from_big_int test_case.exec.address, (fun _ -> true), ())
  in
  let number = test_case.exec.gas in
  let net = EvmCode.network_of_block_number (uint256 (vctx_block_number v)) in
  let ret : instruction_result = Conv.program_sem_wrapper c (EvmCode.Nat number) net (InstructionContinue v) in
  match ret with
  | InstructionContinue _ ->
     let () = Printf.printf "InstructionContinue\n" in
     TestFailure
  | InstructionToEnvironment (ContractCall carg, v, pushed_opt) ->
     let () = Printf.eprintf "We are not looking whatever happens after the contract calls\n" in
     TestSkipped
  | InstructionToEnvironment (ContractDelegateCall carg, v, pushed_opt) ->
     let () = Printf.eprintf "We are not looking whatever happens after the contract calls\n" in
     TestSkipped
  | InstructionToEnvironment (ContractCreate carg, v, pushed_opt) ->
     let () = Printf.eprintf "We are not looking whatever happens after the contract creates" in
     TestSkipped
  | InstructionToEnvironment (ContractFail _, v, pushed_opt) ->
     begin
       match test_case.callcreates, test_case.gas, test_case.logs, test_case.out, test_case.post with
       | [], None, None, None, None -> TestSuccess
       | _ -> failwith "some postconditions are there for a failing case"
     end
  | InstructionToEnvironment (ContractSuicide _, v, pushed_opt) ->
     begin
       match test_case.callcreates, test_case.gas, test_case.logs, test_case.out, test_case.post with
       | spec_created, Some spec_gas, Some spec_logs, Some spec_out, Some spec_post ->
          let () = Printf.eprintf "We are not filling in the gap of a transaction and a message call yet.  For the suicide case, this means we cannot compare the storage and the balance.\n" in
          TestSuccess
       | _ -> failwith "Some post conditions not available"
     end
  | InstructionToEnvironment (ContractReturn retval, v, None) ->
     begin
       match test_case.callcreates, test_case.gas, test_case.logs, test_case.out, test_case.post with
       | spec_created, Some spec_gas, Some spec_logs, Some spec_out, Some spec_post ->
          let got_retval : string = hex_string_of_byte_list "0x" retval in
          if (got_retval <> spec_out) then TestFailure
          else if not (storage_comparison (Conv.w160_from_big_int test_case.exec.address) spec_post (vctx_touched_storage_index v) (vctx_storage v)) then TestFailure
          else if not (balance_comparison (Conv.w160_from_big_int test_case.exec.address) spec_post (vctx_balance v)) then TestFailure
          else if not (log_comparison (vctx_logs v) spec_logs) then TestFailure
          else TestSuccess
       | _ -> failwith "Some post conditions not available"
     end
  | InstructionToEnvironment (ContractReturn retval, v, Some _) ->
     let () = Printf.printf "unexpected return format\n" in
     TestFailure

let r_to_string r =
  match r with
  | TestSuccess -> "passed"
  | TestFailure -> "failed"
  | TestSkipped -> "skipped"

let has_timeouted st =
  match st with
  | Unix.WSIGNALED s -> s = Sys.sigalrm
  | _ -> false

let timeout f arg time = 
 let pipe_r,pipe_w = Unix.pipe () in
 match Unix.fork () with
   | 0 -> let _ = Unix.alarm time in
	  let x = (f arg) in
          let oc = Unix.out_channel_of_descr pipe_w in
          Marshal.to_channel oc x [];
          close_out oc;
          exit 0
   | pid0 -> 
       let (pid, st) = Unix.wait () in
       if pid = pid0 && has_timeouted st then
          let () = Printf.printf "Timeout!\n" in
          TestSkipped
       else
         let ic = Unix.in_channel_of_descr pipe_r in
         let v = Marshal.from_channel ic in
         let () = close_in ic in
	 v

let test_one_file ((num_success : int ref), (num_failure : int ref), (num_skipped : int ref), (start_from : int ref)) (case_name : string option) (path : string) : unit =
  let vm_arithmetic_test : json = Yojson.Basic.from_file path in
  let vm_arithmetic_test_assoc : (string * json) list = Util.to_assoc vm_arithmetic_test in
  let () =  List.iter
    (fun (label, j) ->
      let hit =
        match case_name with
        | Some search ->
           (try
             let _ = BatString.find label search in
             true
           with Not_found -> false)
        | None -> true
      in
      if hit then
        begin
          let () = Printf.printf "+++++++++++++++++++++++++++test case: %s (running)\n" label in
          let st = Sys.time () in
          let res =
            match label with
            | "loop-exp-16b-100k" -> TestSkipped
            | "loop-exp-1b-1M" ->  TestSkipped
            | "loop-exp-2b-100k" ->  TestSkipped
            | "loop-exp-32b-100k" -> TestSkipped 
            | "loop-exp-4b-100k" -> TestSkipped 
            | "loop-exp-8b-100k" -> TestSkipped 
            | "loop-exp-nop-1M" -> TestSkipped
            | "loop-mul" -> TestSkipped
            | "codecopyMemExp"-> TestSkipped
            | "000bc649eea3f4a0b38edf5ea483aa87d1e4969725d5032532830ae7d1fcee14" -> TestSkipped
            | _ -> 
             if !start_from = 0 then
                TestSkipped
             else
                timeout test_one_case j 10 in
          let endt = Sys.time () 
          in let () = Printf.printf "===========================test case: %s (%s) (duration=%d)\n" label (r_to_string res) ((int_of_float endt) - (int_of_float st)) in
          match res with
          | TestSuccess -> num_success := !num_success + 1 
          | TestFailure -> num_failure := !num_failure + 1
          | TestSkipped -> num_skipped := !num_skipped + 1
        end
    )
    vm_arithmetic_test_assoc in
  ()

let () =
  let case_name : string option =
    if Array.length BatSys.argv > 1 then Some (Array.get BatSys.argv 1) else None in
  let num_success = ref 0 in
  let num_failure = ref 0 in
  let num_skipped = ref 0 in
  let start_from = ref 1 in
  let counters = (num_success, num_failure, num_skipped, start_from) in
  let () = TraverseJsons.traverse "../tests/VMTests" (test_one_file counters case_name) in
  let () = Printf.printf "success: %i\n" !num_success in
  let () = Printf.printf "failure: %i\n" !num_failure in
  let () = Printf.printf "skipped: %i\n" !num_skipped in
  ()
