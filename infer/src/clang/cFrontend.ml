(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Ast_utility
module L = Logging

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


(* NOTE: Assumes that an AST always starts with a TranslationUnitDecl *)

let init_global_state_capture () =
  Ident.NameGenerator.reset () ;
  CFrontend_config.global_translation_unit_decls := []

module IDM = Map.Make(String)

let node_map = ref IDM.empty

let node_val = ref 0


let getNodeID node : int  = 
  (* Here is to get the ID, which is unique *)
  let key = (string_of_int (Procdesc.Node.get_id node)) in
  (* Simplification of the identifiers *)
  match IDM.find !node_map key with
  | Some(x) -> x
  | None -> let v = !node_val in 
    incr node_val; 
    let k = v 
    in node_map:= IDM.set !node_map ~key:key ~data:k ; k 


let do_objc_preanalyses cfg tenv =
  if Config.objc_synthesize_dealloc then CAddImplicitDeallocImpl.process cfg tenv ;
  CAddImplicitGettersSetters.process cfg tenv ;
  CReplaceDynamicDispatch.process cfg ;
  CViewControllerLifecycle.process cfg tenv

let rec existStack stack stackIn (t:string) : Exp.t option = 
  match stackIn with 
  | [] -> None 
  | (exp, ident) :: xs  -> 
    if String.compare t (Ident.to_string ident) == 0 
    then 
      let eName = (Exp.to_string exp ) in 
      (match exp with 
      | Lfield (root, field, rest) -> 
        let eNameRoot = (Exp.to_string root ) in 
        let fName = Fieldname.to_string field in  
        if String.compare "n$" (String.sub eNameRoot 0 2 ) == 0 
        then 
          (match existStack stack stack eNameRoot with 
          | None -> Some exp
          | Some exp1 -> Some (Lfield (exp1, field, rest))
          )
          
        else Some exp
      | _ -> 
      if String.compare "n$" (String.sub eName 0 2 ) == 0 
      then existStack stack stack eName
      else Some exp)
    else  existStack stack xs t

let rec expressionToTerm (exp:Exp.t) stack : terms option  = 
  match exp with 
  | Var t -> 
    let tName = (Ident.to_string t) in 
    print_endline ("!!!expressionToTerm tName " ^ tName) ; 
    (match existStack stack stack tName with 
    | Some (Lvar t) -> Some(Basic (BVAR (Pvar.to_string t ))) (** Pure variable: it is not an lvalue *)
    | Some exp -> Some(Basic (BVAR (Exp.to_string exp )))
    | None  ->  Some (Basic (BVAR tName)) (** Pure variable: it is not an lvalue *)
    )
  | Lvar t -> Some (Basic (BVAR (Pvar.to_string t)))  (** The address of a program variable *)

  | Const t ->  (** Constants *)
    (match t with 
    | Cint i -> Some(Basic (BINT (IntLit.to_int_exn i )))  (** integer constants *)
    | _ -> (*Basic BNULL*) None 
    )

  | UnOp (_, t, _) -> 
    (match expressionToTerm t stack with 
    | Some (Basic (BINT n)) -> Some(Basic (BINT ((-1) * n)))
    | _ -> None (*Basic (BVAR ("UnOp1"))*)
    )
    



  | BinOp (Shiftrt, e1, e2)
  | BinOp (MinusA _, e1, e2)
  | BinOp (MinusPI, e1, e2)
  | BinOp (MinusPP, e1, e2) -> 
    let t1 = expressionToTerm e1 stack in 
    let t2 = expressionToTerm e2 stack in 
    (match t1, t2 with 
    | Some t1 , Some t2 -> Some (Minus (t1, t2))
    | _, _  -> None )


    
  | BinOp (Shiftlt, e1, e2)
  | BinOp (PlusA _, e1, e2)
  | BinOp (PlusPI, e1, e2) -> 
    let t1 = expressionToTerm e1 stack in 
    let t2 = expressionToTerm e2 stack in 
    (match t1, t2 with 
    | Some t1 , Some t2 -> Some (Plus (t1, t2))
    | _, _  -> None )

  | BinOp _ (*_ -> Basic (BVAR ("BinOp"))*)
  | Exn _ (*-> Basic (BVAR ("Exn"))*)
  | Closure _ (*-> Basic (BVAR ("Closure"))*)
  | Cast _ (*-> Basic (BVAR ("Cast"))*)
  | Lfield _ (*-> Basic (BVAR ("Lfield"))*)
  | Lindex _ (*-> Basic (BVAR ("Lindex"))*)
  | Sizeof _ -> None (*Basic (BVAR ("Sizeof"))*)


let rec expressionToPure (exp:Exp.t) stack: pure option = 
  print_endline ("expressionToPure : " ^ (Exp.to_string exp));
  match exp with 
  | BinOp (Eq, BinOp (Mod _, e1, e2), e3) ->  
    let t1 = expressionToTerm e1 stack in 
    let t2 = expressionToTerm e2 stack in 
    let t3 = expressionToTerm e3 stack in 
    (match (t1, t2,  t3) with 
    | _ -> None 
    )
  | BinOp (Ne, BinOp (Mod _, e1, e2), e3) ->  
    let t1 = expressionToTerm e1 stack in 
    let t2 = expressionToTerm e2 stack in 
    let t3 = expressionToTerm e3 stack in 
    (match (t1, t2,  t3) with 
    | _ -> None 
    )


  | BinOp (bop, e1, e2) -> 

    let t1 = expressionToTerm e1 stack in 
    let t2 = expressionToTerm e2 stack in 
    (match t1, t2 with 
    | None, _
    | _, None -> None 
    | Some t1 , Some t2 -> 
      let t2 = 
      match t2 with 
      | (Minus(t2, _ )) -> t2 
      | (t2) -> t2 
      in 
      (match bop with 
      | Eq  -> Some (Eq (t1, t2))
      | Lt -> Some (Lt (t1, t2))
      | Gt -> Some (Gt (t1, t2))
      | Le -> Some (LtEq (t1, t2))
      | Ge -> Some (GtEq (t1, t2))
      | Ne -> Some (Neg (Eq (t1, t2)))
      | LAnd | BAnd -> 
        (match expressionToPure e1 stack, expressionToPure e2 stack with 
        | Some p1, Some p2 -> Some (PureAnd (p1, p2))
        | Some p, None | None, Some p -> Some p 
        | _ -> None 
        )
      | LOr | BOr | BXor -> 
        (match expressionToPure e1 stack, expressionToPure e2 stack with 
        | Some p1, Some p2 -> Some (PureOr (p1, p2))
        | Some p, None | None, Some p -> Some p 
        | _ -> None 
        )
      | Shiftrt -> Some (Eq (t1, Minus(t1, t2)))
      | _ -> 
        (*print_endline ("expressionToPure None : " ^ Exp.to_string exp); *)
        None
      ))

  

  | UnOp (_, e, _) -> 
    (match expressionToPure e stack with 
    | Some p -> Some (normalise_pure (Neg p))
    | None -> 
     print_endline ("expressionToPure UnOp : " ^ Exp.to_string exp ); 

        (match expressionToTerm e stack with 
        | Some t -> Some (Eq (t,  Basic (BINT 0)))
        | None -> 
        None 
        )
  
    )
  | Const _ -> 
    if String.compare (Exp.to_string exp) "1" == 0 then Some TRUE
    else 
      (
      print_endline ("expressionToPure Const : " ^ Exp.to_string exp ); 
      None )

  | Lvar _ 
  | Var _ -> 
    (match expressionToTerm exp stack with 
    | Some t -> Some (Neg (Eq (t,  Basic (BINT 0))))
    | None -> None 
    )
  

      
  (*

  print_endline ("expressionToPure Var None : " ^ Exp.to_string exp); 
      None 
  | Exn _ -> 
    print_endline ("expressionToPure Exn None : " ^ Exp.to_string exp); 
    None 
  | Closure _ -> 
    print_endline ("expressionToPure Closure None : " ^ Exp.to_string exp); 
    None 

  | Cast _ -> 
    print_endline ("expressionToPure Cast None : " ^ Exp.to_string exp); 
    None 
  | Lvar _ -> 
    print_endline ("expressionToPure Lvar None : " ^ Exp.to_string exp); 
    None 
  | Lfield _ -> 
    print_endline ("expressionToPure Lfield None : " ^ Exp.to_string exp); 
    None 
      (** A field offset, the type is the surrounding struct type *)
  | Lindex  _ -> 
    print_endline ("expressionToPure Lindex None : " ^ Exp.to_string exp); 
    None 
  | Sizeof  _ -> 
    print_endline ("expressionToPure Sizeof None : " ^ Exp.to_string exp); 
    None 
    *)
  | _ -> 
    (
    None )


let removeDotsInVarName str =
  let str_li =  String.split_on_chars ~on:['.';'&';':'] str in 
  let rec aux li = 
    match li with 
   | [] -> ""
   | [x] -> x
   | x ::xs  -> x ^ "_" ^ aux xs 
   in aux str_li

let rec partitionFromLast (li:'a list) : ('a list * 'a list) = 
  match li with
  | [] -> [], []
  | [x] -> [], [x]
  | x::xs -> 
    let li1, li2 = partitionFromLast xs in 
    x::li1, li2


let updateStakeUsingLoads intrs = 
  List.fold_left intrs ~init:[] ~f:(fun acc (ins:Sil.instr) -> 
    match ins with 
    | Load l -> 
            (*print_endline (Exp.to_string l.e ^ " -> " ^ IR.Ident.to_string l.id); *)
            (l.e, l.id) :: acc 
          | _ -> acc
        ) 


let getPureFromFunctionCall (e_fun:Exp.t) (arg_ts:(Exp.t * Typ.t) list) ((Store s):IR.Sil.instr) stack : pure option =
  let exp1 = s.e1 in 
  let temp = expressionToTerm exp1 stack in 
  match temp with 
  | None -> None 
  | Some temp -> 
    let funName = (Exp.to_string e_fun) in 
    if existAux (fun a b -> String.compare a b == 0) nonDetermineFunCall funName then 
      Some (Eq (temp, Basic(ANY)))
    else 
      (*let argumentTerms =  List.map arg_ts ~f:(fun (eA, _) -> expressionToTerm eA stack) in *)
      (* Predicate(funName, argumentTerms) *)
      Some (Eq (temp, Basic(ANY)))



let rec getPureFromDeclStmtInstructions (instrs:Sil.instr list) stack : pure option = 
  (*print_endline ("getPureFromDeclStmtInstructions: " ^ string_of_int (List.length instrs));
  print_endline (List.fold instrs ~init:"" ~f:(fun acc a -> acc ^ "," ^ string_of_instruction a)); 
  *)
  match instrs with 
  | Store s :: _ -> 
    (*print_endline (Exp.to_string s.e1 ^ " = " ^ Exp.to_string s.e2); *)
    let exp1 = s.e1 in 
    let exp2 = s.e2 in 
    let t1 = expressionToTerm exp1 stack in 
    let t2 = expressionToTerm exp2 stack in 
    (match t1, t2 with 
    | Some (Basic(BSTR a )) , Some (Basic(BINT b )) -> Some (Eq (Basic(BSTR a ), Basic(BINT b )))
    | Some (Basic(BVAR a )) , Some (Basic(BINT b )) -> Some (Eq (Basic(BVAR a ), Basic(BINT b )))
    (*
    | _ -> Some (Eq (t1, Basic ANY))  *)
    (* if it is temp=user_quota_size-quota_size, temp will be ANY *)
    | Some t1, Some t2 -> Some (Eq (t1, t2)) 
    | _, _ -> None 

    )  
    
  | Load l :: tail ->
    let stack' = (l.e, l.id):: stack in 
    getPureFromDeclStmtInstructions tail stack'

  | Call ((ret_id, _), e_fun, arg_ts, _, _)  :: Store s :: _ -> 
    (*print_endline (Exp.to_string e_fun) ;   *)
    getPureFromFunctionCall e_fun arg_ts (Store s) stack
    
  | _ -> None

let rec getPureFromBinaryOperatorStmtInstructions (op: string) (instrs:Sil.instr list) stack : pure option = 
  print_endline ("getPureFromBinaryOperatorStmtInstructions: " ^ string_of_int (List.length instrs));
  
  if String.compare op "Assign" == 0 then 
    match instrs with 
    | Store s :: _ -> 
      print_endline ("Store: " ^  Exp.to_string s.e1 ^ " = " ^ Exp.to_string s.e2); 
      let exp1 = s.e1 in 
      let exp2 = s.e2 in 
      (match expressionToTerm exp1 stack, expressionToTerm exp2 stack with 
      | Some e1, Some e2 -> 
        print_endline ("res = Some " ^ string_of_pure (Eq (e1, e2))); 
        Some (Eq (e1, e2))
      | _, _ -> 
      print_endline ("res = None " ); 
      None 
      
      )
      
    | Load l :: tail ->
      let stack' = (l.e, l.id):: stack in 
      getPureFromBinaryOperatorStmtInstructions "Assign" tail stack'    
    | Call ((ret_id, _), e_fun, arg_ts, _, _)  :: Store s :: _ -> 
      (*print_endline (Exp.to_string e_fun) ;   *)
      getPureFromFunctionCall e_fun arg_ts (Store s) stack
    
    | _ -> None 
  else if String.compare op "SubAssign" == 0 || String.compare op "AddAssign" == 0 then  
    match instrs with 
    | Store s :: _ ->  
      getPureFromBinaryOperatorStmtInstructions "Assign" instrs stack
    | Load l :: tail ->
      let stack' = (l.e, l.id):: stack in 
      print_endline ("SubAssign: " ^ string_of_stack stack');
      getPureFromBinaryOperatorStmtInstructions "SubAssign" tail stack'

    | _ -> None 
  else None


let regularExpr_of_Node node stack : (summary * stack )= 
  let node_kind = Procdesc.Node.get_kind node in
  let node_key =  getNodeID node in
  let instrs_raw =  (Procdesc.Node.get_instrs node) in  
  let instrs = Instrs.fold instrs_raw ~init:[] ~f:(fun acc (a:Sil.instr) -> 
      match a with 
      | Metadata _ -> acc 
      | _ -> acc @ [a]) 
  in 
  match node_kind with
  | Start_node -> [TRUE, Emp], []
  | Exit_node ->  [TRUE, Emp], []
  | Join_node ->  [TRUE, Emp] , []
  | Skip_node _ ->  [TRUE, Emp] , []
  | Prune_node (f,_,_) ->  
    let loads, last = partitionFromLast instrs in 
    let stack' = updateStakeUsingLoads loads in 

    
    (match last with 
    | Prune (e, loc, f, _):: _ ->  
      (match expressionToPure e (stack@stack') with 
      | Some p -> 
        let (p':pure) = 
          match p with 
          | Eq (Basic (BVAR v1), t2) -> 
              let v2= removeDotsInVarName v1 in 
              Eq (Basic (BVAR v2), t2)
          | Neg (Eq (Basic (BVAR v1), t2)) -> 
              let v2= removeDotsInVarName v1 in 
              let p':pure = Neg (Eq (Basic (BVAR v2), t2)) in 
              p'

          | (Gt (Basic (BVAR v1), t2)) -> 
              let v2= removeDotsInVarName v1 in 
              let p':pure = (Gt (Basic (BVAR v2), t2)) in 
              p'

          | (LtEq (Basic (BVAR v1), t2)) -> 
              let v2= removeDotsInVarName v1 in 
              let p':pure = (LtEq (Basic (BVAR v2), t2)) in 
              p'

          | (Lt (Basic (BVAR v1), t2)) -> 
              let v2= removeDotsInVarName v1 in 
              let p':pure = (Lt (Basic (BVAR v2), t2)) in 
              p'

          | (GtEq (Basic (BVAR v1), t2)) -> 
              let v2= removeDotsInVarName v1 in 
              let p':pure = (GtEq (Basic (BVAR v2), t2)) in 
              p'



          | _ -> p 

        in 
        print_endline ("last is Prune " ^ string_of_pure p');
        [(p', Emp)]
      | None -> 

      [(TRUE, Emp)] ), stack'
    | Load l :: Prune (e, loc, f, _):: _ ->  
      let stack' = (l.e, l.id) :: stack in 
      (match expressionToPure e (stack@stack') with 
      | Some p -> [(p, Emp)]
      | None -> 
      [(TRUE, Emp)] ), stack'

    | _ -> 
      [(TRUE, Singleton(TRUE, node_key))] , stack'
    )

  | Stmt_node stmt_kind ->         
    match stmt_kind with 
    | BinaryOperatorStmt (op) -> 
      if existAux (fun a b-> String.compare a b ==0) ["EQ";"GT";"LT";"NE";"LE";"GE"] op then 
        (*String.compare op "EQ" == 0 || String.compare op "GT" == 0 then  *)
        let stack = updateStakeUsingLoads instrs in 
        [(TRUE, Emp)] , stack
        (*Singleton(TRUE, node_key), stack *)
        (* This is to avoid th extra (T)@loc before the guard, we only need to 
           record the stack, but no need any event *)

      else if existAux (fun a b-> String.compare a b ==0) ["ShrAssign"] op then 
        let loads, last = partitionFromLast instrs in 
        let stack' = updateStakeUsingLoads loads in 
        match last with 
        | Store s :: _ -> 
          let exp1 = s.e1 in 
          (match expressionToTerm exp1 stack' with 
          | None -> [(TRUE, Singleton(TRUE, node_key))], []   
          | Some t1 -> 
          
          let g1 =(Lt(t1, Basic (BINT 0 ))) in 
          let g2 =((Gt(t1, Basic (BINT 0 )))) in 

          [(g1, Singleton(Eq(t1, t1), node_key))]
          @
          [(g2, Singleton(Eq(t1, Minus (t1 ,Basic (BINT 1))), node_key))], 
          stack' )
          
        | _ -> ([TRUE, Emp]), []   


          
      else 
        (match getPureFromBinaryOperatorStmtInstructions op instrs stack with 
        | Some pure -> [(TRUE, Singleton (pure, node_key))], []
        | None -> [TRUE, Emp], [] )  
        
    
    | UnaryOperator 
   
    | DeclStmt -> 
      let loads, _ = partitionFromLast instrs in 
      let stack' = updateStakeUsingLoads loads in 

      print_endline ("DeclStmt: " ^ string_of_stack stack');
      (match getPureFromDeclStmtInstructions instrs stack with 
      | Some pure -> [(TRUE, Singleton (pure, node_key))], stack'
      | None -> [TRUE, Emp], stack' )

    | ReturnStmt -> 
      (match instrs with 
      | Store s :: _ -> 
        (*print_endline (Exp.to_string s.e1 ^ " = " ^ Exp.to_string s.e2); *)
        let exp2 = s.e2 in 
        (*predicateDeclearation:= (retKeyword, ["Number";"Number"]) :: !predicateDeclearation ;
        *)
        (match expressionToTerm exp2 stack with
        | Some t -> [(TRUE, Singleton (Predicate (retKeyword, [t]), node_key))], []
        | _ ->  [(TRUE, Singleton (Predicate (retKeyword, []), node_key))], []
        )

      | _ -> 
        [(TRUE, Singleton (Predicate (retKeyword, [Basic(BINT 0)]), node_key))], []
      )
    | Call x  -> 
      (match instrs with 
      | Call ((ret_id, _), e_fun, arg_ts, _, _)  :: _ -> 
        let (argumentTerms:terms list) =  List.fold_left arg_ts ~init:[] ~f:(fun acc (eA, _) -> 
          match expressionToTerm eA stack with 
          | Some t -> acc@[t] 
          | None -> acc 
        ) in 
        let argumentTermsType = List.map argumentTerms 
          ~f:(fun a -> 
          match a with 
          |  (Basic(BINT _ )) ->"Number" 
          |  (Basic(BVAR _ )) -> "Symbol" 
          |  (Basic(BSTR _ )) -> "Symbol" 
          | _ -> "")  in 
        let funName = (Exp.to_string e_fun) in 
        let funName = String.sub funName 5 (String.length funName - 5) in 
        
        let funName =removeDotsInVarName funName in 

        [(TRUE, Singleton (Predicate (funName, argumentTerms), node_key))], [] 
       
      | _ -> 
        let funName = String.sub x 5 (String.length x - 5) in 
        let funName =removeDotsInVarName funName in 

        [(TRUE, Singleton (Predicate (funName, []), node_key))], []
      )
    
    
      
    | _ -> [(TRUE, Emp)] , []

let resolve_loop (loop_body:summary) (rest:summary) : summary = 
  print_endline ("not yet in resolve_loop");
  loop_body @ rest


let rec recordToRegularExpr (li:Procdesc.Node.t list) stack : (summary * stack) = 
  match li with 
  | [] -> [(TRUE, Emp)], []
  | [currentState] -> regularExpr_of_Node currentState stack
  | currentState :: xs  -> 
    let eventHd, stack' = regularExpr_of_Node currentState stack in 
    let eventTail, stack'' = recordToRegularExpr xs (stack@stack') in 
    concateSummaries eventHd eventTail, (stack@stack'@stack'')

let rec existCycleHelper stack (currentState:Procdesc.Node.t) (id:state list) : (summary * stack * bool)  = 
  let node_kind = Procdesc.Node.get_kind currentState in
  let currentID = getNodeID currentState in
  
  
  (*print_endline ("existCycleHelper stack: " ^ string_of_stack stack);*)
  (*
  print_endline ("id:\n" ^  List.fold_left ~init:"" id ~f:(fun acc a -> acc ^ string_of_int (a))); 
  print_endline ("existCycleHelper id: " ^ string_of_int currentID);
  *)
  let idHead, idTail = 
    match id with 
    | [] -> raise (Failure "existCycleHelper not possible")
    | hd::tail -> hd, tail
  in 



  let moveForward_aux stackCtx (nodeIn:Procdesc.Node.t): (summary * stack * bool)  =
    let reExtensionIn, stackIn = recordToRegularExpr ([nodeIn]) stackCtx in  
    let nextStates = Procdesc.Node.get_succs nodeIn in 
    match nextStates with 
    | [] -> 
      (reExtensionIn, stackCtx@stackIn, false)
    
   
    | [succ] -> 
      (match existCycleHelper (stackCtx@stackIn) succ id with 
      | (re, stackSucc, false) -> (concateSummaries reExtensionIn re, stackCtx@stackIn@stackSucc, false) 
      | (re, stackSucc, true) -> (concateSummaries reExtensionIn re, stackCtx@stackIn@stackSucc, true)
      )
    | succ1::succ2::_ -> 
      (match existCycleHelper  (stackCtx@stackIn) succ1 id, existCycleHelper  (stackCtx@stackIn) succ2 id with 
      | (re1, stackSucc1, false), (re2, stackSucc2, false) -> (concateSummaries (reExtensionIn) re1@ re2, stackCtx@stackIn@stackSucc1@stackSucc2, false) 
      | (re1, stackSucc1, false), (re2, stackSucc2, true)
      | (re1, stackSucc1, true), (re2, stackSucc2, false)
      | (re1, stackSucc1, true), (re2, stackSucc2, true) -> (concateSummaries (reExtensionIn) (re1@ re2), stackCtx@stackIn@stackSucc1@stackSucc2, true)
      )

  in 

  if currentID == idHead then ([(TRUE, Emp)], stack, true)
  else if existAux (==) idTail currentID then ([(TRUE, Emp)], stack, false)

  else 
    match node_kind with 
    | Join_node -> 
      (match existCycle stack currentState (currentID::id) with 
      | Some (non_cycle_succ, loop_body, stack1) -> 
        (*print_endline ("loop_body1: " ^ string_of_regularExpr loop_body); *)
        let re1Succ, stackSucc, flag = moveForward_aux (stack@stack1) non_cycle_succ in  
        (resolve_loop loop_body re1Succ , stack@stack1@stackSucc, flag)
      | None -> moveForward_aux stack currentState
      )
    | _ -> moveForward_aux stack currentState





and existCycle stack (currentState:Procdesc.Node.t) (id:state list) : (Procdesc.Node.t * summary * stack) option = 
  
  (*
  print_endline ("existCycl:\n" ^ string_of_int (getLineNum currentState)); 
  print_endline ("id:\n" ^  List.fold_left ~init:"" id ~f:(fun acc a -> acc ^ string_of_int (a))); 
  *)

  let reExtension, stack' = recordToRegularExpr ([currentState]) stack in 


  let nextStates = Procdesc.Node.get_succs currentState in 
  match nextStates with 
  | [succ] -> 
    
    if List.length (Procdesc.Node.get_succs succ) == 1 then None 
    else 
    (match Procdesc.Node.get_kind succ with 
    | Join_node -> None 
    | _ -> 
      (match existCycle (stack'@stack) succ id with 
      | None -> None 
      | Some (node, re, stack'') -> Some (node, concateSummaries  reExtension re, stack@stack'@stack'')
      
      )
    )

    
  | [succ1;succ2] -> 
    let trueNodefalseNode = 
      (match (Procdesc.Node.get_kind succ1, Procdesc.Node.get_kind succ2) with 
      | (Prune_node(true, _, _), Prune_node(false, _, _)) -> Some (succ1, succ2)
      | (Prune_node(false, _, _), Prune_node(true, _, _)) -> Some (succ2, succ1)
      | _ -> None
      )
    in 
    (match  trueNodefalseNode with 
    | None -> None 
    | Some (trueNode, falseNode) -> 
      (match existCycleHelper (stack@stack') trueNode id with 
      | (_, _, false) -> None 
      | (re, stack'', true) -> Some (falseNode, re, stack@stack'@stack'')
      )
  )
    
  | _ -> None 


  

let rec getRegularExprFromCFG_helper stack (currentState:Procdesc.Node.t): (summary * stack) = 
  let node_kind = Procdesc.Node.get_kind currentState in
  let currentID = getNodeID currentState in
  (*print_endline ("getRegularExprFromCFG_helper:\n" ^ string_of_int currentID); 
  *)

  let moveForward stackCtx (nodeIn:Procdesc.Node.t): (summary * stack)  = 
    let reExtensionIn, stackIn = recordToRegularExpr ([nodeIn]) stackCtx in 

    let stack'' = (stackIn@stackCtx) in 
    let nextStates = Procdesc.Node.get_succs nodeIn in 
    match nextStates with 
    | [] -> (reExtensionIn , stack'')
    | [succ] ->  
      let re1Succ, stackSucc= getRegularExprFromCFG_helper stack'' succ in 
      concateSummaries reExtensionIn re1Succ, (stack''@stackSucc)

    | succ1::succ2::_ -> 
      let re1Succ1, stackSucc1 = getRegularExprFromCFG_helper stack'' succ1 in 
      let re1Succ2, stackSucc2 = getRegularExprFromCFG_helper stack'' succ2 in 
      concateSummaries (reExtensionIn) (re1Succ1@re1Succ2), (stack''@stackSucc1@stackSucc2)
  in 

  (match node_kind with 

  | Exit_node | Stmt_node ReturnStmt -> (* looping at the last state *)
    let reExtension, stack' = recordToRegularExpr ([currentState]) stack in 
    (reExtension, (stack@stack'))
  | Join_node ->
    (match existCycle stack currentState [currentID] with 
    | Some (non_cycle_succ, loop_body, stack1) -> 
      (*print_endline ("loop_body2: " ^ string_of_regularExpr loop_body); *)
      let re1Succ, stackSucc = moveForward (stack@stack1) non_cycle_succ in 
      (resolve_loop loop_body re1Succ) , (stack@stack1@stackSucc)
    | None -> moveForward stack currentState
    )
  | _ -> moveForward stack currentState
  )



let getRegularExprFromCFG (procedure:Procdesc.t) : summary = 
  let startState = Procdesc.get_start_node procedure in 
  let r, _ = getRegularExprFromCFG_helper [] startState in 
  r



let computeSummaryFromCGF (procedure:Procdesc.t) : (summary) = 
  let pass1 =  normalise_summary (getRegularExprFromCFG procedure) in 
  
  print_endline ("\nPASS1:\n"^string_of_summary (pass1)^ "\n------------"); 


  pass1




let do_cpp_preanalyses cfg = CppLambdaCalls.process cfg

let do_source_file (translation_unit_context : CFrontend_config.translation_unit_context) ast =
  let tenv = Tenv.create () in
  CType_decl.add_predefined_types tenv ;
  init_global_state_capture () ;
  let source_file = translation_unit_context.CFrontend_config.source_file in
  (*let integer_type_widths = translation_unit_context.CFrontend_config.integer_type_widths in
  L.(debug Capture Verbose)
    "@\n Start building call/cfg graph for '%a'....@\n" SourceFile.pp source_file ;
  *)

  print_endline ("File analysed : \"" ^ SourceFile.to_string source_file ^ "\"");  
  let cfg = compute_icfg translation_unit_context tenv ast in


  let summaries = (Cfg.fold_sorted cfg ~init:[] 
  ~f:(fun accs procedure -> 
    print_endline ("\n//-------------\nFor procedure: " ^ Procname.to_string (Procdesc.get_proc_name procedure) ^":" );
    let (summary:summary) = computeSummaryFromCGF procedure in 
    List.append accs [summary] )) 
  in

  let _ = List.iter ~f:(fun a -> print_endline (Ast_utility.string_of_summary a)) summaries in 

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