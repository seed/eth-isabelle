open Yojson.Basic
open Stateparser
(* open Block *)
open EvmCode
open TestResult


let bighex x = Z.format "%x" (Z.of_string (Big_int.string_of_big_int x))
let big_int_to_Z v = Z.of_string (Big_int.string_of_big_int v)

let construct_block_info (t : test_case) : unit block_info_ext =
  let open VmTestParser in
  let block_number = Conv.w256_from_big_int t.env.currentNumber in
  Block_info_ext (
  (fun num ->
      let num : Big_int.big_int = Conv.uint256_big num in
      if Big_int.lt_big_int num (Big_int.sub_big_int t.env.currentNumber (Big_int.big_int_of_int 256)) then
        Conv.w256_from_big_int Big_int.zero_big_int
      else if Big_int.gt_big_int num t.env.currentNumber then
        Conv.w256_from_big_int Big_int.zero_big_int
      else if Big_int.eq_big_int num t.env.currentNumber then
        Conv.w256_from_big_int Big_int.zero_big_int
      else
        let hashed_byte_list = (Conv.string_as_byte_list
                       (Big_int.string_of_big_int num)) in
        let ret = EvmCode.keccak hashed_byte_list in
        ret
    )
  , Conv.w160_from_big_int t.env.currentCoinbase
  , Conv.w256_from_big_int t.env.currentTimestamp
  , block_number
  , Conv.w256_from_big_int t.env.currentDifficulty
  , Conv.w256_from_big_int t.env.currentGasLimit
  , ())

let string_to_w256 str = Conv.w256_from_big_int (Big_int.big_int_of_string str)

let zero = string_to_w256 "0"

let convert_storage lst =
  let conv = List.map (fun (p,v) -> (Conv.w256_from_big_int p, string_to_w256 v)) lst in
  (fun x -> try List.assoc x conv with _ -> zero)

let convert_state addr st =
  Account_ext 
( addr
, convert_storage st.VmTestParser.storage
, fst (Hexparser.parse_code st.VmTestParser.code)
, Conv.w256_from_big_int st.VmTestParser.balance
, Conv.w256_from_big_int st.VmTestParser.nonce
, true
, false
, ())

let make_state_list lst =
  List.map (fun (a,st) ->
     let addr = Conv.w160_from_big_int (Big_int.big_int_of_string ("0x"^a)) in
     let stor_lst = List.map (fun (p,v) -> (Conv.w256_from_big_int p, string_to_w256 v)) st.VmTestParser.storage in
     (addr, convert_state addr st, stor_lst)) lst

let construct_tr (a:Stateparser.tr) = 
Transaction_ext 
( Conv.w160_from_big_int a.address
, (match a.target with None -> None | Some x -> Some (Conv.w160_from_big_int x))
, Conv.w256_from_big_int a.gasLimit
, Conv.w256_from_big_int a.gasPrice
, Conv.w256_from_big_int a.value
, Conv.w256_from_big_int a.nonce
, Conv.byte_list_of_hex_string a.data
, ())

let w256hex i = Z.format "%x" (Word256.word256ToNatural i)
let w256dec i = Z.format "%d" (Word256.word256ToNatural i)

let debug_vm c1 pr =
 match pr with
  | InstructionContinue v ->
     prerr_endline ("Gas " ^ Big_int.string_of_big_int (Conv.tobigint (RunVmTest.vctx_gas v)));
     (match vctx_next_instruction v c1 with
      | None -> ()
      | Some i ->
        (* prerr_endline ("Watch " ^ w256hex (v.vctx_storage (Word256.word256FromNat 1))); *)
        (* prerr_endline ("Calldata " ^ String.concat "," (List.map (fun x -> Z.format "%x" (Word8.word8ToNatural x)) v.vctx_data_sent)); *)
        prerr_endline ("Inst " ^ String.concat "," (List.map (fun x -> Z.format "%x" (big_int_to_Z (Conv.uint8_big x))) (inst_code i)));
        prerr_endline ("Stack " ^ String.concat "," (List.map (fun x -> Z.format "%d" (big_int_to_Z (Conv.uint256_big x))) (RunVmTest.vctx_stack v))) )
  | InstructionToEnvironment( _, v, _) -> prerr_endline ("Gas left " ^ Z.to_string (big_int_to_Z (Conv.tobigint (RunVmTest.vctx_gas v))))

  (*
let debug_state = function
 | Continue res -> debug_vm res.g_cctx res.g_vmstate
 | _ -> ()
*)

let debug_mode = if Array.length Sys.argv > 2 then true else false

exception Skip

let run_tr tr state block net =
  let res = start_transaction tr state block in
  let rec do_run = function
   | Finished fi -> fi
   | Unimplemented -> raise Skip
   | a ->
     (*if debug_mode then debug_state a; *)
     do_run (next0 net a) in
  let fi = do_run res in
  (*
  if debug_mode then begin
    prerr_endline ("Bal " ^ w256dec (fi.f_state tr.tr_from).account_balance0);
    prerr_endline ("Killed " ^ string_of_int (List.length fi.f_killed));
  end;
  *)
  let final_state = end_transaction fi tr block in
  final_state

let compare_storage diff_found a stor (p,v) =
  if stor p <> v then begin
      Printf.printf "address %s has storage %s at %s, but it should be %s!\n" (Conv.string_of_address a)
       (Conv.decimal_of_w256 (stor p)) (Conv.decimal_of_w256 p) (Conv.decimal_of_w256 v);
      diff_found := true
  end

let blk_info_block_number bi = 
   match bi with 
   Block_info_ext (_,_,_,block_number,_,_,_) ->
           block_number

let account_balance0 a = 
   match a with 
   Account_ext (_,_,_,balance,_,_,_,_) ->
           balance

let account_nonce a = 
   match a with 
   Account_ext (_,_,_,_,nonce,_,_,_) ->
           nonce

let account_storage0 a = 
   match a with 
   Account_ext (_,storage0,_,_,_,_,_,_) ->
           storage0

let run_test (label, elm) : testResult =
  let () = Printf.printf "%s\n%!" label in
  let tc = parse_test_case elm in
  let block_info = construct_block_info tc in
  let tr = construct_tr tc.tr in
  let pre_st = List.map (fun (a,b,_) -> (a,b)) (make_state_list tc.pre) in
  let post_st = make_state_list tc.post in
  let state x = try List.assoc x pre_st with _ -> empty_account0 x in
  let net = EvmCode.network_of_block_number (uint256 (blk_info_block_number block_info)) in
  try
    let state = run_tr tr state block_info net in
    let diff_found = ref false in
    List.iter (fun (a,cmp, storage_list) ->
      let acc = state a in
      if account_balance0 acc <> account_balance0 cmp then begin
        Printf.printf "address %s has balance %s, but it should be %s!\n%!" (Conv.string_of_address a) (Conv.decimal_of_w256 (account_balance0 acc))
         (Conv.decimal_of_w256 (account_balance0 cmp));
        diff_found := true
      end;
      if account_nonce acc<> account_nonce cmp then begin
        Printf.printf "address %s has nonce %s, but it should be %s!\n%!" (Conv.string_of_address a) (Conv.decimal_of_w256 (account_nonce acc))
         (Conv.decimal_of_w256 (account_nonce cmp));
         diff_found := true
      end;
      List.iter (compare_storage diff_found a (account_storage0 acc)) storage_list) post_st;
    (if !diff_found then TestFailure else TestSuccess)
  with
    Skip -> TestSkipped
