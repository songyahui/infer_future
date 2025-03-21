(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module TestInterpreter = AnalyzerTester.Make (AddressTaken.TransferFunctions (ProcCfg.Exceptional))

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let assert_empty = invariant "{ }" in
  let int_typ = Typ.mk (Tint IInt) in
  let int_ptr_typ = Typ.mk (Tptr (int_typ, Pk_pointer)) in
  let fun_ptr_typ = Typ.mk (Tptr (Typ.mk Tfun, Pk_pointer)) in
  let closure_exp captureds =
    let mk_captured_var str =
      ( Exp.Var (ident_of_str str)
      , { CapturedVar.pvar= pvar_of_str str
        ; typ= int_ptr_typ
        ; capture_mode= CapturedVar.ByReference
        ; is_formal= None } )
    in
    let captured_vars = List.map ~f:mk_captured_var captureds in
    let closure = {Exp.name= dummy_procname; captured_vars} in
    Exp.Closure closure
  in
  let test_list =
    [ ( "address_taken_set_instr"
      , [var_assign_addrof_var ~rhs_typ:int_ptr_typ "a" "b"; invariant "{ &b }"] )
    ; ("address_not_taken_set_instr", [var_assign_addrof_var ~rhs_typ:int_typ "a" "b"; assert_empty])
    ; ("address_not_taken_load_instr1", [id_assign_var ~rhs_typ:int_ptr_typ "a" "b"; assert_empty])
    ; ("address_not_taken_load_instr2", [id_assign_var ~rhs_typ:int_typ "a" "b"; assert_empty])
    ; ( "take_multiple_addresses"
      , [ var_assign_addrof_var ~rhs_typ:int_ptr_typ "a" "b"
        ; invariant "{ &b }"
        ; var_assign_addrof_var ~rhs_typ:int_ptr_typ "c" "d"
        ; invariant "{ &d, &b }"
        ; var_assign_addrof_var ~rhs_typ:int_typ "e" "f"
        ; invariant "{ &d, &b }" ] )
    ; ( "address_not_taken_closure"
      , [ var_assign_addrof_var ~rhs_typ:int_ptr_typ "a" "b"
        ; var_assign_exp ~rhs_typ:fun_ptr_typ "c" (closure_exp ["d"; "e"])
        ; invariant "{ &b }" ] )
    ; ( "if_conservative1"
      , [ If (unknown_exp, [var_assign_addrof_var ~rhs_typ:int_ptr_typ "a" "b"], [])
        ; invariant "{ &b }" ] )
    ; ( "if_conservative2"
      , [ If
            ( unknown_exp
            , [var_assign_addrof_var ~rhs_typ:int_ptr_typ "a" "b"]
            , [var_assign_addrof_var ~rhs_typ:int_ptr_typ "c" "d"] )
        ; invariant "{ &d, &b }" ] )
    ; ( "loop_as_if"
      , [ While (unknown_exp, [var_assign_addrof_var ~rhs_typ:int_ptr_typ "a" "b"])
        ; invariant "{ &b }" ] )
    ; ( "loop_before_after"
      , [ var_assign_addrof_var ~rhs_typ:int_ptr_typ "a" "b"
        ; invariant "{ &b }"
        ; While (unknown_exp, [var_assign_addrof_var ~rhs_typ:int_ptr_typ "c" "d"])
        ; invariant "{ &d, &b }"
        ; var_assign_addrof_var ~rhs_typ:int_ptr_typ "e" "f"
        ; invariant "{ &d, &b, &f }" ] ) ]
    |> TestInterpreter.create_tests (fun _summary -> ()) ~initial:AddressTaken.Domain.empty
  in
  "address_taken_suite" >::: test_list
