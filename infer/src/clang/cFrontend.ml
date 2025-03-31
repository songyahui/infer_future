(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Ast_utility
open Sys
module L = Logging
open Format

(* ocamlc gets confused by [module rec]: https://caml.inria.fr/mantis/view.php?id=6714 *)
(* it also ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)
include struct
  [@@@warning "-unused-module"]

  module rec CTransImpl : CModule_type.CTranslation = CTrans.CTrans_funct (CFrontend_declImpl)

  and CFrontend_declImpl : CModule_type.CFrontend = CFrontend_decl.CFrontend_decl_funct (CTransImpl)
end

(* Translates a file by translating the ast into a cfg. *)
let compute_icfg trans_unit_ctx tenv ast =
  match ast with
  | Clang_ast_t.TranslationUnitDecl (_, decl_list, _, _) ->
      CFrontend_config.global_translation_unit_decls := decl_list ;
      L.(debug Capture Verbose) "@\n Start creating icfg@\n" ;
      let cfg = Cfg.create () in
      List.iter
        ~f:(CFrontend_declImpl.translate_one_declaration trans_unit_ctx tenv cfg `DeclTraversal)
        decl_list ;
      L.(debug Capture Verbose) "@\n Finished creating icfg@\n" ;
      cfg
  | _ ->
      assert false

let string_of_unary_operator_info  (unary_operator_info:Clang_ast_t.unary_operator_info)= 
  match unary_operator_info.uoi_kind with
    `PostInc -> "PostInc"
    | `PostDec -> "PostDec"
    | `PreInc -> "PreInc"
    | `PreDec -> "PreDec"
    | `AddrOf -> "AddrOf"
    | `Deref -> "Deref"
    | `Plus-> "Plus"
    | `Minus -> "Minus"
    | `Not -> "Not"
    | `LNot -> "LNot"
    | `Real -> "Real"
    | `Imag -> "Imag"
    | `Extension -> "Extension"
    | `Coawait-> "Coawait"

let string_of_binary_operator (binary_operator_kind:Clang_ast_t.binary_operator_info)= 
  match binary_operator_kind.boi_kind with
  | `PtrMemD -> "PtrMemD"
  | `PtrMemI -> "PtrMemI"
  | `Mul -> "Mul"
  | `Div -> "Div"
  | `Rem -> "Rem"
  | `Add -> "Add"
  | `Sub -> "Sub"
  | `Shl -> "Shl"
  | `Shr-> "Shr"
  | `Cmp -> "Cmp"
  | `LT -> "LT"
  | `GT -> "GT"
  | `LE -> "LE"
  | `GE -> "GE"
  | `EQ -> "EQ"
  | `NE -> "NE"
  | `And -> "And"
  | `Xor -> "Xor"
  | `Or -> "Or"
  | `LAnd-> "LAnd"
  | `LOr -> "LOr"
  | `Assign -> "Assign"
  | `MulAssign -> "MulAssign"
  | `DivAssign -> "DivAssign"
  | `RemAssign -> "RemAssign"
  | `AddAssign-> "AddAssign"
  | `SubAssign -> "SubAssign"
  | `ShlAssign -> "ShlAssign"
  | `ShrAssign -> "ShrAssign"
  | `AndAssign -> "AndAssign"
  | `XorAssign-> "XorAssign"
  | `OrAssign -> "OrAssign"
  | `Comma-> "Comma"




let stmt2Term_helper (op: Clang_ast_t.binary_operator_info) (t1: term) (t2: term) : term = 
  (match op.boi_kind with 
  | `Add ->  (Plus (t1, t2))
  | `Sub ->  (Minus (t1, t2))
  | `Mul ->  (TTimes (t1, t2))
  | `Div->  (TDiv (t1, t2))
  | _ -> (TApp("stmt2Term_helper", [Var (string_of_binary_operator (op))])) 
  )
    


    

let rec stmt2Term (instr: Clang_ast_t.stmt) : term = 
  (*print_endline ("term kind:" ^ Clang_ast_proj.get_stmt_kind_string instr);*)
  match instr with 
  | ImplicitCastExpr (_, x::rest, _, _, _) 
  | AtomicExpr(_, x::rest, _, _) 
  | UnaryExprOrTypeTraitExpr(_, x::rest, _, _) 
  | CStyleCastExpr (_, x::rest, _, _, _) 
  | ParenExpr (_, x::rest, _) -> 
  (*print_string ("ParenExpr/CStyleCastExpr: " ^ (
    List.fold_left (x::rest) ~init:"" ~f:(fun acc a -> acc ^ "," ^ Clang_ast_proj.get_stmt_kind_string a)
  )^ "\n");
  *)

    stmt2Term x
  
  | BinaryOperator (stmt_info, x::y::_, expr_info, binop_info)->
    stmt2Term_helper binop_info (stmt2Term x) (stmt2Term y) 
  | IntegerLiteral (_, stmt_list, expr_info, integer_literal_info) ->
    let int_str = integer_literal_info.ili_value in 

    if String.length int_str > 18 then ((Var "SYH_BIGINT"))
    else ((Num (int_of_string(int_str))))
      (*Some ((Num (int_of_string(int_str))))*)
    
  | DeclRefExpr (stmt_info, _, expr_info, decl_ref_expr_info) -> 
    let (sl1, sl2) = stmt_info.si_source_range in 

    (match decl_ref_expr_info.drti_decl_ref with 
    | None -> 
     ((Var(Clang_ast_proj.get_stmt_kind_string instr ^"(!!!stmt2Term, DeclRefExpr)"))) 
    | Some decl_ref ->
      (
      match decl_ref.dr_name with 
      | None -> ((Var(Clang_ast_proj.get_stmt_kind_string instr ^"(!!!stmt2Term, DeclRefExpr2)"))) 
      | Some named_decl_info -> 
        let c_type = CAst_utils.get_type expr_info.ei_qual_type.qt_type_ptr in 
        
        (match c_type with 
          | Some (PointerType _) -> (((* Pointer *) Var (named_decl_info.ni_name)))
            
          | _ -> ((Var (named_decl_info.ni_name)))

        )
        
      
      )
    )
  | NullStmt _ -> (Nil)

  | ArraySubscriptExpr (_, arlist, _)  -> 

    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 
    (*print_endline (string_of_int (List.length temp)); *)
   ((Var(string_with_seperator  (fun a -> (string_of_term a)) temp ".")))

  | MemberExpr (_, arlist, _, member_expr_info)  -> 
    let memArg = member_expr_info.mei_name.ni_name in 
    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 

    let name  = string_with_seperator (fun a ->(string_of_term a)) temp "." in 
    if String.compare memArg "" == 0 then ((Var(name )))
    else ((Member(Var name, [Var memArg])))


  | UnaryOperator (stmt_info, x::_, expr_info, op_info) ->
    (match op_info.uoi_kind with
    | `Minus -> 
      (match stmt2Term x with 
      | ( (Num t)) -> ((Num (0-t)))
      | _ -> 
        stmt2Term x

      )
      
    | _ -> 
      stmt2Term x
    )
   

  | RecoveryExpr (_, [], _) -> ((Num(0))) 
    

  | ConditionalOperator (_, x::y::_, _) -> stmt2Term y 
  | StringLiteral (_, _, _, _)
  | CharacterLiteral _ -> ((Var "char")) 

  | CallExpr (_, stmt_list, ei) -> 
    (match stmt_list with
    | [] -> assert false 
    | x :: rest -> 
    ((TApp(string_of_stmt x, List.map rest ~f:(fun a -> stmt2Term a))))  
    )

  | _ -> ((Var(Clang_ast_proj.get_stmt_kind_string instr ^"(!!!stmt2Term)"))) 


and string_of_decl (dec :Clang_ast_t.decl) : string = 
  match dec with 
  | VarDecl (_, ndi, qt, vdi) -> 
    ndi.ni_name ^ "::" ^ Clang_ast_extend.type_ptr_to_string qt.qt_type_ptr
    ^" "^ (match vdi.vdi_init_expr with 
    | None -> " none"
    | Some stmt -> " " ^ string_of_stmt stmt)

    (* clang_prt_raw 1305- int, 901 - char *)
  | _ ->  Clang_ast_proj.get_decl_kind_string dec

and string_of_stmt_list (li: Clang_ast_t.stmt list) sep : string = 
    match li with 
  | [] -> ""
  | [x] -> string_of_stmt x 
  | x::xs -> string_of_stmt x ^ sep ^ string_of_stmt_list xs sep

and string_of_stmt (instr: Clang_ast_t.stmt) : string = 
  let rec helper_decl li sep = 
    match li with 
  | [] -> ""
  | [x] -> string_of_decl  x 
  | x::xs -> string_of_decl  x ^ sep ^ helper_decl xs sep
  in 
(*
  let rec helper li sep = 
    match li with 
  | [] -> ""
  | [x] -> string_of_stmt x 
  | x::xs -> string_of_stmt x ^ sep ^ helper xs sep
  in 
*)
  match instr with 
  | ReturnStmt (stmt_info, stmt_list) ->
    "ReturnStmt " ^ string_of_stmt_list stmt_list " " 

  | ArraySubscriptExpr (_, arlist, _)  -> 
    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 
    (*print_endline (string_of_int (List.length temp)); *)
    string_with_seperator  (fun t -> (string_of_term t)) temp "."

  | MemberExpr (_, arlist, _, member_expr_info)  -> 
    let memArg = member_expr_info.mei_name.ni_name in 
    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 

    let name  = string_with_seperator string_of_term temp "." in 
    if String.compare memArg "" == 0 then name 
    else name ^ "." ^ memArg

  | IntegerLiteral (_, stmt_list, expr_info, integer_literal_info) ->
    (*"IntegerLiteral " ^*) integer_literal_info.ili_value

  | StringLiteral (_, stmt_list, expr_info, str_list) -> 
    let rec straux li = 
      match li with 
      | [] -> ""
      | x :: xs  -> x  ^ " " ^ straux xs 
    in (* "StringLiteral " ^ string_of_int (List.length stmt_list)  ^ ": " ^ *) straux str_list


  | UnaryOperator (stmt_info, stmt_list, expr_info, unary_operator_info) ->
    (*"UnaryOperator " ^*) string_of_stmt_list stmt_list " " ^ ""
  
  | ImplicitCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) -> 
    (*"ImplicitCastExpr " ^*) string_of_stmt_list stmt_list " " 
  | DeclRefExpr (stmt_info, _, _, decl_ref_expr_info) ->
    (*"DeclRefExpr "^*)
    (match decl_ref_expr_info.drti_decl_ref with 
    | None -> "none"
    | Some decl_ref ->
      (
        match decl_ref.dr_name with 
        | None -> "none1"
        | Some named_decl_info -> named_decl_info.ni_name
      )
    )

  | ParenExpr (stmt_info (*{Clang_ast_t.si_source_range} *), stmt_list, _) ->

    string_of_stmt_list stmt_list " " 

    
  | CStyleCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) -> 
    string_of_stmt_list stmt_list " " ^ ""


  | IfStmt (stmt_info, stmt_list, if_stmt_info) ->

  "IfStmt " ^ string_of_stmt_list stmt_list "," ^ ""
 
  | CompoundStmt (_, stmt_list) -> string_of_stmt_list stmt_list "; " 

  | BinaryOperator (stmt_info, stmt_list, expr_info, binop_info) -> 
   "BinaryOperator " ^ string_of_stmt_list stmt_list (" "^ Clang_ast_proj.string_of_binop_kind binop_info.boi_kind ^" ")  ^""

  | DeclStmt (stmt_info, stmt_list, decl_list) -> 
  "DeclStmt " (*  ^ string_of_stmt_list stmt_list " " ^ "\n"^
    "/\\ "^ string_of_int stmt_info.si_pointer^ " " *)  ^ helper_decl decl_list " " ^ "" 
  
  | CallExpr (stmt_info, stmt_list, ei) -> 
      (match stmt_list with
      | [] -> assert false 
      | x :: rest -> 
    "CallExpr " ^ string_of_stmt x ^" (" ^  string_of_stmt_list rest ", " ^ ") "
)

  | ForStmt (stmt_info, stmt_list) ->
    "ForStmt " ^  string_of_stmt_list (stmt_list) " " 

  
  | WhileStmt (stmt_info, stmt_list) ->
    "WhileStmt " ^  string_of_stmt_list (stmt_list) " " 

  | RecoveryExpr (stmt_info, x::_, _) -> "RecoveryExpr " ^ string_of_stmt x
  | RecoveryExpr (stmt_info, [], _) -> "RecoveryExpr []" 

  | BreakStmt _ -> "BreakStmt"
  | _ -> "string_of_stmt not yet " ^ Clang_ast_proj.get_stmt_kind_string instr;;



let stmt2Pure_helper (op: string) (t1: term) (t2: term) : pure option = 

  let p = 
      if String.compare op "<" == 0 then Lt (t1, t2)
    else if String.compare op ">" == 0 then Gt (t1, t2)
    else if String.compare op ">=" == 0 then GtEq (t1, t2)
    else if String.compare op "<=" == 0 then LtEq (t1, t2)
    else if String.compare op "!=" == 0 then Neg (Eq (t1, t2))

    else Eq (t1, t2)
  in Some p 



let rec stmt2Pure (instr: Clang_ast_t.stmt) : pure option = 
  (*print_string ("stmt2Pure" ^ Clang_ast_proj.get_stmt_kind_string instr );*)
  match instr with 
  | BinaryOperator (stmt_info, x::y::_, expr_info, binop_info)->
    (match binop_info.boi_kind with
    | `LT -> stmt2Pure_helper "<" (stmt2Term x) (stmt2Term y) 
    | `GT -> stmt2Pure_helper ">" (stmt2Term x) (stmt2Term y) 
    | `GE -> stmt2Pure_helper ">=" (stmt2Term x) (stmt2Term y) 
    | `LE -> stmt2Pure_helper "<=" (stmt2Term x) (stmt2Term y) 
    | `EQ -> stmt2Pure_helper "=" (stmt2Term x) (stmt2Term y) 
    | `NE -> stmt2Pure_helper "!=" (stmt2Term x) (stmt2Term y) 
    | `And | `LAnd -> 
      (match ((stmt2Pure x ), (stmt2Pure y )) with 
      | Some p1, Some p2 -> Some (Ast_utility.PureAnd (p1, p2))
      | Some p1, None -> Some (p1)
      | None, Some p1 -> Some (p1)
      | None, None -> None 
      )
    | `Or | `LOr | `Xor-> 
      (match ((stmt2Pure x ), (stmt2Pure y )) with 
      | Some p1, Some p2 -> Some (Ast_utility.PureOr (p1, p2))
      | Some p1, None -> Some (p1)
      | None, Some p1 -> Some (p1)
      | None, None -> None 
      )
    | _ -> None 
    )

  | ImplicitCastExpr (_, x::_, _, _, _) -> stmt2Pure x
  | UnaryOperator (stmt_info, x::_, expr_info, op_info)->
    (match op_info.uoi_kind with
    | `Not -> 
      (match stmt2Pure x with 
      | None -> (*print_endline ("`Not none");*) None 
      | Some p -> Some (Neg p))
    | `LNot -> 
      (match stmt2Term x with 
      |  t -> Some (Eq(t, (Num 0)))
      )
      
    | _ -> 
      None
    )
  | ParenExpr (_, x::rest, _) -> stmt2Pure x
  | MemberExpr _ -> 
    (match stmt2Term instr with 
    |  t -> Some (Gt (t, (Num 0))) )
  | DeclRefExpr _ -> 
    (match stmt2Term instr with 
    |  t -> Some (Neg(Eq(t, (Num 0))))
    )

  | IntegerLiteral _ -> 
    if String.compare (string_of_stmt instr) "0" == 0 then Some (FALSE)
    else if String.compare (string_of_stmt instr) "1" == 0 then Some (TRUE)
    else None 
  | NullStmt _ -> Some (Ast_utility.FALSE)

  
  | _ -> Some (Gt ((( Var (Clang_ast_proj.get_stmt_kind_string instr))), ( Var ("null"))))


let si_source_location_to_int (s:Clang_ast_t.source_location) = 
  (match s.sl_line with 
  | None -> -1
  | Some n -> n )

let stmt_intfor2FootPrint (stmt_info:Clang_ast_t.stmt_info): (int) = 
  let ((sl1, _)) = stmt_info.si_source_range in 
    (* let (lineLoc:int option) = sl1.sl_line in *)
  si_source_location_to_int sl1
  



let enforecePureSingleEffect (p:pure) (re:singleEffect) : singleEffect = 
  let (exs, a, b, c, r, errorCode) = re in 
  (exs, PureAnd(a, p), b, c, r, errorCode)
  

let enforecePure (p:pure) (re:effect) : effect = 
  List.map re ~f:(fun single -> enforecePureSingleEffect  p single ) 



let rec findSpecifictaionSummaries (f:string) (summaries:summary list) : summary option = 
  match summaries with 
  | [] -> None 
  | ((fname, args), preC, re) :: xs  -> 
    if String.compare fname f ==0 then Some ((fname, args), preC, re)
    else findSpecifictaionSummaries f xs 

let add_exs exs1 effect : effect = 
  List.map ~f:(fun (exs, p, re, fc, r, errorCode) -> (exs@exs1, p, re, fc, r, errorCode)) effect 

let rec trace_subtraction_single (p:pure) (fc: regularExpr) (es:regularExpr) : regularExpr =
  match es with 
  | Emp -> fc 
  | Bot -> Bot 
  | _ -> 
    let fst = re_fst es in 
    let res = List.fold_left ~f:(fun acc ev -> 
      let derive1 = derivative p ev fc in 
      let derive2 = derivative p ev es in  
      let temp = trace_subtraction_single p derive1 derive2 in
      Disjunction (acc, temp)) ~init:Bot fst  in 
    let res = normalise_es res  in 
    res
    
    

let trace_subtraction (lhsp:pure) (rhsp:pure) (fc: futureCond) (es:regularExpr) (fp:int): futureCond =
  debug_printTraceSubtraction ( "======="); 
  debug_printTraceSubtraction (string_of_pure lhsp ^ " - " ^ string_of_pure rhsp);
  debug_printTraceSubtraction (string_of_fc fc ^ " - " ^ string_of_regularExpr es);
  
  let res = normalise_fc (List.map ~f:(fun a -> 

    let p =  (PureAnd(lhsp, rhsp)) in 
    let single_res = trace_subtraction_single p a es in 
    (match single_res with | Bot -> 
      error_message ("\nThe future condition is violated here at line " ^ string_of_int fp ^ "\n  Future condition is = " ^ string_of_regularExpr a ^ "\n  Trace subtracted = " ^ string_of_regularExpr es ^ "\n  Pure =  " ^ string_of_pure p);  
    | _ -> ()
    ); 
    
    

    single_res

    
    ) fc) in 
  debug_printTraceSubtraction ("res = " ^ string_of_fc res ^ "\n");
  res

let rec compose_effects (eff:singleEffect) eff2 (fp:int) : effect = 
    match eff2 with
    | [] -> []
    | x :: xs -> 
      let (exs, p, re, fc, _, ec1) = eff in 
      if ec1 < 0 then eff :: compose_effects eff xs fp 
      else 
        let (exs', p', re', fc', ret', ec2) = x in 
        let fc_subtracted = normalise_fc (trace_subtraction p p' (removeAny fc) re' fp) in 
        let retFinal, errorCode = match fc_subtracted with 
        | [Bot] -> Var "_", errorCode_exit
        | _ -> ret', ec2
        in 
        (exs@exs', PureAnd(p, p'), Concate(re, re'), fc_subtracted@fc', retFinal, errorCode) :: compose_effects eff xs fp 


let string2TermLi li = (List.map ~f:(fun a -> Var a) li) 

let substitute_single_effect_renaming (spec:singleEffect) (mappings:((term*term)list)): singleEffect = 
  let (exs, p, es, fc, ret, errorCode) = spec in 
  let (newexs:string list) = List.map ~f:(fun a -> verifier_get_A_freeVar (Var a)) exs in 
  let (exs_mappsing : ((term*term)list)) = actual_formal_mappings (string2TermLi newexs) (string2TermLi exs) in 
  let (substitute_foramlArgs_and_exs:singleEffect) = substitute_single_effect (newexs, p, es, fc, ret, errorCode) (mappings@ exs_mappsing) in 

  let (_, _, _, _, ret', _) = substitute_foramlArgs_and_exs in 

  match ret' with 
  | Var _ ->  (* if the callee return a var than get a fresh variable *)
    let alltheFormalArgs = List.map ~f:(fun (a, _) -> a) mappings in 
    if (* if teh return val is one of the formalArgs, then no need to rename *)
      existAux strict_compare_Term alltheFormalArgs ret' then substitute_foramlArgs_and_exs
    else 
      let newr = verifier_get_A_freeVar ret' in 
      let (exs'', p'', es'', fc'', ret'', errorCode'') = (substitute_single_effect substitute_foramlArgs_and_exs [(Var newr, ret')]) in 
      (exs''@[newr], p'', es'', fc'', ret'', errorCode'')

    (* if not, no need to change it *)
  | _ -> substitute_foramlArgs_and_exs


let renameALLLocalVar(summary:summary) :summary = 
  
  (*

  let ((name, old_formal_Args), preCond, effects) = summary in

  let (new_formal_Args:term list) = List.map ~f:(fun a -> Var (verifier_get_A_freeVar a)) old_formal_Args in 
  let mappings = actual_formal_mappings new_formal_Args old_formal_Args in 
  let preCond' = substitute_pure preCond mappings in
  let effects' = substitute_effect effects mappings in 

    let effects'' = List.map ~f:(fun (exs, p, es, fc) -> 
  ) effects' in 
*)
  summary

  (*
  match preCond with 
  | Exists (strLi, pIn) -> 
    let (newexs:string list) = List.map ~f:(fun a -> verifier_get_A_freeVar (Var a)) strLi in 
    let string2Term li = (List.map ~f:(fun a -> Var a) li) in   
    let (exs_mappsing : ((term*term)list)) = actual_formal_mappings (string2Term newexs) (string2Term strLi) in 
    let preCond' =  Exists (newexs, substitute_pure pIn exs_mappsing) in 
    let effects' = substitute_effect effects exs_mappsing in 
    (signature, preCond', effects'), newexs
  | _ -> summary, []
  *)

  

let rec forward_reasoning (signature:signature) (states:effect) (prog: core_lang) : effect = 

  let rec aux expr (state:singleEffect) : effect = 
  let (exs, p, re, fc, ret, errorCode) = state in
  

  match expr with 


  | CValue(t, fp) -> 
    let final = [(exs, p, re, fc, t, errorCode)] in 
    final 
  
  | CSeq (e1, e2) -> 
    let effect1 = aux e1 state in
    let effect2 = forward_reasoning signature effect1 e2 in
    effect2 


  | CAssign (v, e, fp) -> 
    let effect1 = aux e state in 

    let effect1', extensionR = 
      (* check if v has occurred before or not *)
        let allTerms_fromPure = getAllTermsFromPure p in 
        if existAux (fun t1 t2 -> strict_compare_Term t1 t2 ) allTerms_fromPure v 
        then (* if yes, then rename it to r *)
          let r = verifier_get_A_freeVar v in 
          substitute_effect effect1 [(Var r, v)], [r]
        else (* if no, just leave it and there is no fresh variable crated  *)
          effect1, [] in 
    List.map ~f:(fun (exs, p, re, fc, ret, errorCode) -> (exs@extensionR, PureAnd(p, Eq(v, ret)), re, fc, UNIT, errorCode)) effect1'
    
  | CLocal (str, fp) -> [(exs@[str], p, re, fc, ret, errorCode)]

  | CIfELse (p, e1, e2, _) -> 
    let current1 = enforecePure p [state] in 
    let current2 = enforecePure (Neg p) [state] in 
    let effect1 = forward_reasoning signature current1 e1 in 
    let effect2 = forward_reasoning signature current2 e2 in
    (*debug_printCIfELse("effect1 = " ^ string_of_effect effect1) ; 
    debug_printCIfELse("effect2 = " ^ string_of_effect effect2) ; 
    *)
    effect1@effect2

  | CFunCall (f, xs, fp) -> 
    let f_summary = findSpecifictaionSummaries f !summaries in
    (match f_summary with 
    | None -> [state]
    | Some summary -> 
      let summary = renameALLLocalVar summary in 
      debug_printCFunCall ("current state : " ^ string_of_effect [state]); 
      debug_printCFunCall ("actual args : " ^ string_of_li (fun a-> string_of_term a) xs ","); 
      debug_printCFunCall ("callee spec : " ^ string_of_summary summary); 

      let (_, foramlArgs), preC, postSummary = summary in 

      let mappings = (actual_formal_mappings xs foramlArgs)  in 

      let constriants4Mapping = List.fold_left ~f:(fun acc (a, f) -> PureAnd(acc, Eq(a, f))) mappings ~init:TRUE in 

      (* Check pre condition *) 
      (match (checkPreCondition (PureAnd (p, constriants4Mapping)) preC) with 
      | None -> (debug_printCFunCall ("checkPreCondition ERROR!");
                [([], FALSE, Bot, [], Var "_", -1)])
      | Some resdue -> 
        debug_printCFunCall ("residue: " ^ string_of_pure resdue);
        let state = enforecePureSingleEffect resdue state in 
        
        (* substitute the formal arguments and rename all the existential variables  *) 
        let substitutedPostSummary = List.fold_left 
          ~f:(fun acc spec -> acc@ [substitute_single_effect_renaming spec mappings]) ~init:[] postSummary  in 
        debug_printCFunCall ("substitutedSummary : " ^ string_of_effect substitutedPostSummary); 

        (* Compose post condition *) 
        let composeStates = (compose_effects state substitutedPostSummary fp) in 
        debug_printCFunCall ("composeStates : " ^ string_of_effect composeStates); 

        composeStates
      )
    )
  | CWhile (guard, body, fp) -> 
      
      let invariant = invariantInference state guard body in  
      (match invariant with 
      | None -> 
        error_message("\nLoop Invariants generation failed at line " ^ string_of_int fp );
        [(exs, p, re, fc, ret, -1)]
      | Some effInv -> [state]
      )
      
  | _ -> [state]
  in 
  List.fold_left ~f:(fun acc (a :singleEffect )-> 
    let (_, _ ,_, _, _,  errorCode) = a in 
    if errorCode < 0 then acc @ [a] 
    else acc @ aux prog a) ~init:[] (normalise_effect states) 




let rec extractEventFromFUnctionCall (x:Clang_ast_t.stmt) (rest:Clang_ast_t.stmt list) : event option = 
(match x with
| DeclRefExpr (stmt_info, _, _, decl_ref_expr_info) -> 
  let (sl1, sl2) = stmt_info.si_source_range in 
  let (lineLoc:int option) = sl1.sl_line in 

  (match decl_ref_expr_info.drti_decl_ref with 
  | None -> None  
  | Some decl_ref ->
    (
    match decl_ref.dr_name with 
    | None -> None 
    | Some named_decl_info -> 
      Some (Pos (named_decl_info.ni_name, ((List.map rest ~f:stmt2Term))))
    )
  )

| ImplicitCastExpr (_, stmt_list, _, _, _) ->
  (match stmt_list with 
  | [] -> None 
  | y :: restY -> extractEventFromFUnctionCall y rest)

| BinaryOperator (_, x::_, _, _)
| ParenExpr (_, x::_, _) -> extractEventFromFUnctionCall x rest
| (CallExpr (_, stmt_list, _)) -> 
  (match stmt_list with 
  | [] -> None 
  | x::rest -> extractEventFromFUnctionCall x rest
  )

| _ -> 
  None 
)

let loop_guard condition = 
  match stmt2Pure condition with 
    | None -> print_endline ("loop guard error " ^ string_of_stmt condition); TRUE
    | Some p -> p


let rec creatIntermidiateValue4execution (li:term list) state : ((core_lang list) * (term list)) = 
  match li with 
  | [] -> [], []
  | x :: xs  -> 
    let cl1, tLi1  = 
      match x with 
      | TApp (str, terms) -> 
        let ex = Var (verifier_get_A_freeVar UNIT) in 
        [(CAssign(ex, CFunCall(str, terms, state), state))], [ex]
      | _ -> [], [x]
        
    in 
    
    let cl2, tLi2  = (creatIntermidiateValue4execution xs state) in 
    cl1@cl2, tLi1@tLi2




let rec convert_AST_to_core_program (instr: Clang_ast_t.stmt)  : core_lang = 

  let sequentialComposingListStmt (stmts: core_lang list) fp =

    let rec composeStmtList (li:core_lang list): core_lang = 
      match li with 
      | [] -> CValue (UNIT, fp) 
      | [x] -> x 
      | x :: xs -> CSeq(x, composeStmtList xs) 
    in 
    composeStmtList stmts
    
  in 

  let core_lang_of_decl_list (dec :Clang_ast_t.decl list) fp : core_lang = 
    let reverseDeclList = reverse dec in 
    let rec assambleThePairs (li:Clang_ast_t.decl list) (acc: core_lang option): ((string * core_lang option * int) list) = 
      match li with 
      | [] -> [] 
      | x :: xs  -> 
        (match x with 
        | VarDecl (_, ndi, qt, vdi) -> 
          let varName = ndi.ni_name in 
          let (acc': core_lang option) = 
            (match vdi.vdi_init_expr with 
            | None -> acc
            | Some stmt -> Some (convert_AST_to_core_program stmt))
          in 
          (varName, acc', fp) :: assambleThePairs (xs) acc'
        | _ -> []
  
        )
    in 
    let (temp:((string * core_lang option * int) list)) = assambleThePairs reverseDeclList None in 
    let reverseBack = reverse temp in 
    let (coreLangList:core_lang list) = flattenList (List.map reverseBack ~f:(fun (a, b, c) -> 
      match b with 
      | None -> [CLocal(a, c)]
      | Some e -> [CLocal(a, c); CAssign (Var a, e, c)]
      ))
    in sequentialComposingListStmt coreLangList fp

  in   
  
  
  match instr with 

  | IntegerLiteral (stmt_info, stmt_list, expr_info, integer_literal_info) ->
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 

    let int_str = integer_literal_info.ili_value in 

    if String.length int_str > 18 then (CValue (Var "SYH_BIGINT", fp))
    else (CValue(Num (int_of_string(int_str)), fp))


  | ParenExpr(stmt_info, stmt_list, _) 
  | ImplicitCastExpr (stmt_info, stmt_list, _, _, _) 
  | CStyleCastExpr (stmt_info, stmt_list, _, _, _) 
  | CompoundStmt (stmt_info, stmt_list) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    let stmts = List.map stmt_list ~f:(fun a -> convert_AST_to_core_program a) in 
    sequentialComposingListStmt stmts fp 

  | ReturnStmt  (stmt_info, stmt_list) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    let terms = List.map stmt_list ~f:(fun a -> stmt2Term a) in 

    (
    match terms with
    | [] -> (CFunCall("return", [UNIT], fp))
    | x :: _ -> (CFunCall("return", [x], fp))
    | _ -> (CFunCall("return", [TList(terms)], fp))
    )

    
    




  | CompoundAssignOperator (stmt_info, x::y::_, _, _, _) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    (match stmt2Term x, stmt2Term y with 
    |  t1 ,  t2 -> CAssign (t1, CValue (Plus(t1, t2), fp), fp)

    )
       

  | DeclRefExpr (stmt_info, _, _, decl_ref_expr_info) ->
    (*"DeclRefExpr "^*)
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    (match decl_ref_expr_info.drti_decl_ref with 
    | None ->
      CFunCall ((Clang_ast_proj.get_stmt_kind_string instr, [], fp))

    | Some decl_ref ->
      (
        match decl_ref.dr_name with 
        | None -> 
          CFunCall ((Clang_ast_proj.get_stmt_kind_string instr, [], fp)) 
        | Some named_decl_info -> (CValue (Var named_decl_info.ni_name, fp))
      )
    )


  | DeclStmt (stmt_info, _, handlers) -> 

    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    core_lang_of_decl_list handlers fp

    

  | WhileStmt (stmt_info, condition:: stmt_list) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    let (loop_guard:pure) = loop_guard condition in 
    let stmts = List.map stmt_list ~f:(fun a -> convert_AST_to_core_program a) in 

    let core_lang = sequentialComposingListStmt stmts fp in 
    CWhile (loop_guard, core_lang, fp)

  | (IfStmt (stmt_info, condition:: stmt_list, _))  -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    let (conditional_guard:pure) = loop_guard condition in 
    let( (e1, e2 ) : (core_lang * core_lang)) = 
      match stmt_list with 
      | [] -> ( CValue (UNIT, fp), CValue (UNIT, fp))
      | [x] -> (convert_AST_to_core_program x, CValue (UNIT, fp))
      | x :: y :: _ -> (convert_AST_to_core_program x, convert_AST_to_core_program y)
    in 
    CIfELse (conditional_guard, e1, e2, fp)


  | UnaryOperator (stmt_info, x::_, expr_info, unary_operator_info) ->
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    let varFromX = stmt2Term x 
    in 
    
    let (e:core_lang) = 
      (match unary_operator_info.uoi_kind with
      | `PreInc 
      | `PostInc -> CAssign(varFromX, CValue (Plus(varFromX, Num 1), fp), fp)
      | `PreDec
      | `PostDec -> CAssign(varFromX, CValue (Minus(varFromX, Num 1), fp), fp)
      | `AddrOf 
      | `Deref -> CValue (varFromX, fp)
      | `Minus -> CValue (TTimes(Num (-1), varFromX), fp)
      | _ -> 
        print_endline (string_of_unary_operator_info unary_operator_info); 
        CFunCall ((Clang_ast_proj.get_stmt_kind_string instr, [Var (string_of_unary_operator_info unary_operator_info)], fp)) 
      )
    in e 

  | BinaryOperator (stmt_info, [x], _, binop_info)->
    convert_AST_to_core_program x

  | BinaryOperator (stmt_info, x::y::_, _, binop_info)->
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 

    (match stmt2Term x, stmt2Term y with 
    |  t1 ,  t2 -> 
      (match binop_info.boi_kind with
      | `Add -> CValue (Plus (t1, t2), fp)
      | `Sub -> CValue (Minus (t1, t2), fp)
      | `Assign -> 
        (match t2 with 
        | TApp (op, args) ->  CAssign (t1, CFunCall (op, args, fp), fp)
        | _ -> CAssign (t1, CValue (t2, fp), fp)
        )
        
      | `MulAssign -> CAssign (t1, CValue (TTimes(t1, t2), fp), fp)
      | `DivAssign -> CAssign (t1, CValue (TDiv(t1, t2), fp), fp)
      | `AddAssign-> CAssign (t1, CValue (Plus(t1, t2), fp), fp)
      | `SubAssign -> CAssign (t1, CValue (Minus(t1, t2), fp), fp)
      | `And 
      | `Or 
      | `NE 
      | `EQ ->
        let (p:pure) = loop_guard instr in 
        CIfELse(p, CValue(Num 1, fp), CValue(Num 0, fp), fp)


 
      | _ ->  
        print_endline (string_of_binary_operator binop_info); 
        CFunCall ((Clang_ast_proj.get_stmt_kind_string instr, [Var (string_of_binary_operator binop_info)], fp)) 
      )
    
    )

  | MemberExpr (stmt_info, arlist, _, member_expr_info)  -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    let memArg = member_expr_info.mei_name.ni_name in 
    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 
    if String.compare memArg "" == 0 then CValue ((Var(memArg ), fp))
    else CValue((Member(Var memArg, temp), fp))



    
  | ForStmt (stmt_info, init:: decl_stmt:: condition:: update:: body) ->
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    (*
    print_endline ("decl_stmt " ^ Clang_ast_proj.get_stmt_kind_string decl_stmt); 
    (*it is usuallt decl_stmt NullStmt *)
    *)

    let init = convert_AST_to_core_program init in 
    let (loop_guard:pure) = loop_guard condition in 
    let update = convert_AST_to_core_program update in  

    let body = List.map body ~f:(fun a -> convert_AST_to_core_program a) in 
    let loop_body = sequentialComposingListStmt body fp in 
    let loop = CWhile (loop_guard, CSeq(loop_body, update), fp)  in 


    CSeq(init, CSeq(loop_body, loop))

  | DoStmt (stmt_info, body::condition::_) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    let (loop_guard:pure) = loop_guard condition in 
    let body = List.map [body] ~f:(fun a -> convert_AST_to_core_program a) in 
    let loop_body = sequentialComposingListStmt body fp in 

    let loop = CWhile (loop_guard, loop_body, fp)  in 
    CSeq(loop_body, loop)


  | LabelStmt (stmt_info, stmt_list, label_name) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    let stmts = List.map stmt_list ~f:(fun a -> convert_AST_to_core_program a) in 
    let core_lang = sequentialComposingListStmt stmts fp in 
    CSeq(CLable (label_name, fp), core_lang)

  | GotoStmt (stmt_info, _, {Clang_ast_t.gsi_label= label_name; _}) ->
    (* print_endline ("goto: " ^ label_name);  *) 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    CGoto (label_name, fp)

  | ContinueStmt (stmt_info, _) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    CContinue (fp)
  | BreakStmt (stmt_info, _)  -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    CBreak (fp)

  | CallExpr (stmt_info, x ::rest, ei) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    (match extractEventFromFUnctionCall x rest with 
    | Some (Pos(calleeName, acturelli)) -> (* arli is the actual argument *)
      if existAux (fun a b -> String.compare a b == 0) nonDetermineFunCall calleeName then CValue (ANY, fp)
      else 
        let prefixCmds, acturelli' = creatIntermidiateValue4execution acturelli fp in 
        sequentialComposingListStmt (prefixCmds@[(CFunCall(calleeName, acturelli', fp))]) fp
    | _ -> 
        let stmts = List.map (x ::rest) ~f:(fun a -> convert_AST_to_core_program a) in sequentialComposingListStmt stmts fp
    )
  | NullStmt (stmt_info, _) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    CValue (Nil, fp)

  | CXXConstructExpr (stmt_info, stmt_list, expr_info, cxx_construct_expr_info) -> 
    let (fp:int) = stmt_intfor2FootPrint stmt_info in 
    (*print_endline (string_of_int (List.length stmt_list));  *)
    CValue (ANY, fp)
    (*  struct st p  *)

  | _ -> 
    CFunCall((Clang_ast_proj.get_stmt_kind_string instr, [], -1)) 


let vardecl2String_with_type (dec: Clang_ast_t.decl): (string * (string list * pure) option)  = 
  match dec with 
  | VarDecl (decl_info,  named_decl_info,  qual_type , var_decl_info)
  | ParmVarDecl (decl_info,  named_decl_info,  qual_type ,  var_decl_info) -> 
    let varName = named_decl_info.ni_name in 
    
    let c_type = CAst_utils.get_type qual_type.qt_type_ptr in 
    let preC = 
      match c_type with 
      | Some (PointerType _) -> 
        let r = verifier_get_A_freeVar (Var "loc") in 
        Some ( [r], Eq (Var varName, Var r))

        (*let type_str = Clang_ast_j.string_of_c_type c_type in 
        let name = named_decl_info.ni_name in 
        type_str ^ " " ^ name
        *)
      | Some _ (* BuiltinType *)
      | None -> None
    in 
    varName, preC
  | _ -> Clang_ast_proj.get_decl_kind_string dec, None 


let vardecl2String (dec: Clang_ast_t.decl): term  = 
  match dec with 
  | VarDecl (decl_info,  named_decl_info,  qual_type , var_decl_info)
  | ParmVarDecl (decl_info,  named_decl_info,  qual_type ,  var_decl_info) -> 
    let varName = named_decl_info.ni_name in 
    let c_type = CAst_utils.get_type qual_type.qt_type_ptr in 
        
        (match c_type with 
          | Some (PointerType _) -> (* Pointer *) Var varName
            
          | _ -> Var varName

        )
        
    
  | _ -> Var (Clang_ast_proj.get_decl_kind_string dec) 


let sysfile (str:string) : bool  = 
  if String.length str < 1 then false 
  else 
    let sub = String.sub str 0 1 in 
    String.compare sub "_" == 0
    

let reason_about_declaration (dec: Clang_ast_t.decl) (source_Address:string): unit = 
  match dec with
    | FunctionDecl ((* decl_info *) _, named_decl_info, _, function_decl_info) ->
      (
      match function_decl_info.fdi_body with 
      | None -> ()
      | Some stmt -> 

        let funcName = named_decl_info.ni_name in 

        if sysfile funcName then () (*print_endline ("skipping " ^ funcName) *)
        else

          (*debug_print (source_Address);  *)

          (let (parameters: term list ) = List.map (function_decl_info.fdi_parameters) ~f:(fun a -> (vardecl2String a)) in 
          

          debug_print ("\n~~~~~~~~~~~~\n" ^ "annalysing " ^ funcName ^ "(" ^ string_of_li (fun a -> string_of_term a) parameters "," ^ ")");
          (*debug_print (source_Address);  *)
          let (startingState:effect) = [defultSingelEff] in

          let (core_prog:core_lang) = convert_AST_to_core_program stmt in 
          
          let (stmt_info , _) =  Clang_ast_proj.get_stmt_tuple stmt in 
          let (_, s2) = stmt_info.si_source_range in 


          let (fp:int) =si_source_location_to_int s2 in 

          debug_print (string_of_core_lang core_prog);
          let signature = (funcName, parameters) in 

          let raw_final = normalise_effect (forward_reasoning signature startingState core_prog) in 
          
          debug_print("\nRaw_final  = " ^ string_of_effect raw_final);
          let (postProcess:effect) = normalise_effect ((postProcess raw_final)) in

          debug_print("\nPostProcess= " ^ string_of_effect postProcess);
          let resetErrorCodeEffect = List.fold_left ~f:(
            fun acc (a, b, c, d, e, f) -> 
              let extra = 
                if f == errorCode_return then [(a, b, c, d, e, 0)] (* reset the ones for return *)
                else  [(a, b, c, d, e, f)]               
              in 
              acc@ extra)
            ~init:[] postProcess in 



          let final  = checkPostConditionError resetErrorCodeEffect parameters fp in 
          

          let (summary:summary) =  signature, TRUE, final in 

          summaries := !summaries @ [(summary)])
      

      )

    | _ -> () 



let syhtrim str =
  if String.compare str "" == 0 then "" else
  let search_pos init p next =
    let rec search i =
      if p i then raise(Failure "empty") else
      match str.[i] with
      | ' ' | '\n' | '\r' | '\t' -> search (next i)
      | _ -> i
    in
    search init
  in
  let len = String.length str in
  try
    let left = search_pos 0 (fun i -> i >= len) (succ)
    and right = search_pos (len - 1) (fun i -> i < 0) (pred)
    in
    String.sub str left (right - left + 1)
  with
  | Failure "empty" -> ""
;;

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (syhtrim line) :: input_lines file
  | _ -> assert false 
;;
let retriveLinesOfCode (source:string) : (int) = 
  let ic = open_in source in
  try
      let lines =  (input_lines ic ) in
      let rec helper (li:string list) = 
        match li with 
        | [] -> ""
        | x :: xs -> x ^ "\n" ^ helper xs 
      in       
      let line_of_code = List.length lines in 
      (line_of_code)


    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;

let retrive_basic_info_from_AST ast_decl: (string * Clang_ast_t.decl list * int) = 
    match ast_decl with
    | Clang_ast_t.TranslationUnitDecl (decl_info, decl_list, _, translation_unit_decl_info) ->
        let source =  translation_unit_decl_info.tudi_input_path in 
        let lines_of_code  = retriveLinesOfCode source in 
        (source, decl_list, lines_of_code) 
 
    | _ -> assert false

let init_global_state_capture () =
  Ident.NameGenerator.reset () ;
  CFrontend_config.global_translation_unit_decls := []


let do_cpp_preanalyses cfg = CppLambdaCalls.process cfg


let outputFinalReport str path = 

  let oc = open_out_gen [Open_append; Open_creat] 0o666 path in 
  try 
    Printf.fprintf oc "%s" str;
    close_out oc;
    ()

  with e ->                      (* 一些不可预见的异常发生 *)
    print_endline ("could not open " ^ path);
    close_out_noerr oc;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
  ;; 


let create_newdir path = 
  let full_permission = 0o777 in
  try 
    if (Sys.is_directory path) == `No then 
      Unix.mkdir path ~perm:full_permission
  with Sys_error _ -> ()

let retriveComments (source:string) : (string list) = 
  (*print_endline (source); *) 
  let partitions = Str.split (Str.regexp "/\*@") source in 
  (* print_endline (string_of_int (List.length partitions)); *)
  match partitions with 
  | [] -> [](*assert false*) 
  | _ :: rest -> (*  SYH: Note that specification can't start from line 1 *)
  let partitionEnd = List.map rest ~f:(fun a -> Str.split (Str.regexp "@\*/")  a) in 
  let rec helper (li: string list list): string list = 
    match li with 
    | [] -> []
    | x :: xs  -> 
      (match List.hd x with
      | None -> helper xs 
      | Some head -> 
        if String.compare head "" ==0 then helper xs 
        else 
          let ele = ("/*@" ^ head ^ "@*/") in 
          (ele :: helper xs)  ) 
  in 
  let temp = helper partitionEnd in 
  temp
  

let retriveSpecifications (source:string) : (Ast_utility.summary list * int * int) = 
  
  try
    let ic = open_in source in
    let lines =  (input_lines ic ) in
    let rec helper (li:string list) = 
      match li with 
      | [] -> ""
      | x :: xs -> x ^ "\n" ^ helper xs 
    in 
    let line = helper lines in
      
    let partitions = retriveComments line in (*in *)
    let line_of_spec = List.fold_left partitions ~init:0 ~f:(fun acc a -> acc + (List.length (Str.split (Str.regexp "\n") a)))  in 
      
    (*
    (if List.length partitions == 0 then ()
    else debug_print ("Global specifictaions are: ")); 
    *)

    let user_sepcifications = List.map partitions 
        ~f:(fun singlespec -> 
        (*debug_print ("singlespec: " ^ singlespec ^ "\n");   *)
          let summaries = Parser.summary Lexer.token (Lexing.from_string singlespec) in 
          (*debug_print (string_of_summary summaries); *)

          summaries
          
    ) in

    summaries := !summaries @ user_sepcifications ; 

      
      (*
      let _ = List.map sepcifications ~f:(fun (_ , pre, post, future) -> print_endline (string_of_function_sepc (pre, post, future) ) ) in 
      *)

      close_in ic ;                 (* 关闭输入通道 *)
      (user_sepcifications, line_of_spec, List.length partitions)

      (*
            flush stdin;                (* 现在写入默认设备 *)
      print_string (List.fold_left (fun acc a -> acc ^ forward_verification a progs) "" progs ) ; 
      *)

    with e ->                      (* 一些不可预见的异常发生 *)
      debug_print ("Something wrong in parsing  " ^ source);
      ([], 0, 0)

   ;;

let max_syh a b = if a > b then a else b

let rec map2 f l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | h1::t1, h2::t2 -> (f h1 h2) :: map2 f t1 t2
  | _, _ -> []

let rec iter2 f l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | h1::t1, h2::t2 -> f h1 h2; iter2 f t1 t2
  | _, _ -> ()

let print_table ~headers (rows:string list list) =
  (* Calculate maximum column widths *)
  let (col_widths:int list) = 
    List.fold_left ~f:(fun (acc:int list) (row:string list) -> 
      map2 (fun w s -> max_syh w (String.length s)) acc row 
    ) ~init:(List.map ~f:String.length headers) rows in
  
  let print_separator () =
    printf "+";
    List.iter ~f:(fun w -> printf "%s+" (String.make (w + 2) '-')) col_widths;
    printf "@."
  in
  
  let print_row row =
    printf "|";
    iter2 (fun w cell -> printf " %-*s |" w cell) col_widths row;
    printf "@."
  in
  
  print_separator ();
  print_row headers;
  print_separator ();
  List.iter ~f:print_row rows;
  print_separator ()


let partition_at i lst =
  let (left, right, _) = 
    List.fold_left ~f:(fun (left, right, idx) x ->
      if idx < i then (x::left, right, idx+1)
      else (left, x::right, idx+1)
    ) ~init:([], [], 0) lst
  in
  (List.rev left, List.rev right)

let rec print_out_the_inferred_specifications summaries num : unit = 
  let (_, right) = partition_at num summaries in
  List.iter ~f:(fun a -> report_print (string_of_summary a))  right


let do_source_file (translation_unit_context : CFrontend_config.translation_unit_context) ast =
  verifier_counter_reset_to 0; 

  let tenv = Tenv.create () in
  CType_decl.add_predefined_types tenv ;
  init_global_state_capture () ;
  let source_file = SourceFile.to_string  (translation_unit_context.CFrontend_config.source_file) in
  (* let cfg = compute_icfg translation_unit_context tenv ast in
  let integer_type_widths = translation_unit_context.CFrontend_config.integer_type_widths in
  L.(debug Capture Verbose)
    "@\n Start building call/cfg graph for '%a'....@\n" SourceFile.pp source_file ;
  *)
  current_source_file := source_file ; 

  print_endline ("File analysed : \"" ^ source_file ^ "\"\n");  

  let (source_Address, decl_list, lines_of_code) = retrive_basic_info_from_AST ast in


  let path = Sys.getcwd () in
  let (_, lines_of_spec_macro, number_of_protocol_macro) = retriveSpecifications (path ^ "/spec.c") in 
  
  let (_, lines_of_spec_local, number_of_protocol_local) = retriveSpecifications (source_file) in 

  let start = Unix.gettimeofday () in 
  let _ = List.iter decl_list  
    ~f:(fun dec -> reason_about_declaration dec source_Address) in 
  let analysisTime = (Unix.gettimeofday () -. start) in 

  let msg = 
    source_Address ^ ","
  ^ string_of_int (lines_of_code + 1 ) ^ "," (*  lines of code;  *) 
  ^ string_of_float (analysisTime)^ "," (* "Analysis took "^ , seconds.\n\n *)
  in 

  report_print ("\n+--------------------+-----------------+"); 
  report_print ("|        Inferred Specifications       |"); 
  report_print ("+--------------------+-----------------+"); 
  print_out_the_inferred_specifications !summaries (number_of_protocol_macro + number_of_protocol_local); 


  let () = finalReport := !finalReport ^ msg ^ !errormessage in 
  let dirName = "/infer-term" in 
  let path = Sys.getcwd()  (* "/Users/yahuis/Desktop/git/infer_termination/" *) in 
  print_endline (Sys.getcwd());

  create_newdir (path ^ dirName); 

  let output_report =  path ^ dirName ^ "/report.csv" in 
  let output_detail =  path ^ dirName ^ "/detail.txt" in 
  

  outputFinalReport (msg) output_report ; 
  (
  if String.compare !finalReport "" == 0  then ()
  else   
    (let () = finalReport := ("\nIn " ^ source_Address ^ ":\n") ^ !finalReport  in 
    outputFinalReport (!finalReport) output_detail)) ; 

  print_table ~headers:["Summary"; ""]
  [
    ["No. Premitive Spec"; string_of_int (number_of_protocol_macro + number_of_protocol_local)];
    ["No. Inferred Spec"; string_of_int (List.length !summaries - (number_of_protocol_macro + number_of_protocol_local))];
    ["Lines of Spec"; string_of_int (lines_of_spec_macro + lines_of_spec_local)];
    ["Lines of Code"; string_of_int (lines_of_code + 1 - lines_of_spec_local )];
    ["Analysis Time"; string_of_float (analysisTime) ^ "s"];
    ["No. Violation"; string_of_int (!errormessagecounter) ];
    ["Report File"; path ^ dirName ^ "/detail.txt" ];
  ];

  ()


(*
  Config.inline_func_pointer_for_testing
  |> Option.iter ~f:(fun prefix -> CMockPointerSubst.process cfg prefix) ;
  ( match translation_unit_context.CFrontend_config.lang with
  | CFrontend_config.ObjC | CFrontend_config.ObjCPP ->
      do_objc_preanalyses cfg tenv
  | _ ->
      () ) ;
  ( match translation_unit_context.CFrontend_config.lang with
  | CFrontend_config.CPP | CFrontend_config.ObjCPP ->
      do_cpp_preanalyses cfg
  | _ ->
      () ) ;
  L.(debug Capture Verbose) "@\n End building call/cfg graph for '%a'.@\n" SourceFile.pp source_file ;
  SourceFiles.add source_file cfg (Tenv.FileLocal tenv) (Some integer_type_widths) ;
  if Config.debug_mode then Tenv.store_debug_file_for_source source_file tenv ;
  if
    Config.debug_mode || Config.testing_mode || Config.frontend_tests
    || Option.is_some Config.icfg_dotty_outfile
  then DotCfg.emit_frontend_cfg source_file cfg ;
  L.debug Capture Verbose "Stored on disk:@[<v>%a@]@." Cfg.pp_proc_signatures cfg ;
  ()
*)