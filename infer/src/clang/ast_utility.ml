open Z3


let errorCode_normal = 0 
let errorCode_return = -1 
let errorCode_exit = -2 

let errorCode_failure = -3

let errorCode_break = -4



let retKeyword = "Return"
let finalReport = (ref "")
let verifier_counter: int ref = ref 0;;


let debug_print str = 
  if true then print_endline (str)
  else ()

let debug_printCFunCall str = 
  if false then debug_print (str)
  else ()

let debug_printTraceSubtraction str = 
  if false then debug_print (str)
  else ()


let debug_printCIfELse str = 
  if false then debug_print (str)
  else ()

let debug_postprocess str = 
    if false then debug_print (str)
    else ()

let debug_Inv_Infer str = 
    if false then debug_print (str)
    else ()
  

let debug_checkPostConditionError str = 
    if false then debug_print (str)
    else ()
  
let debug_derivative str = 
    if true then debug_print (str)
    else ()

let report_print str = 
    if true then print_endline (str)
    else ()
  
    
    

let errormessage = ref ""
let errormessagecounter = ref 0


let error_message str = 
  errormessagecounter := !errormessagecounter + 1 ; 
  errormessage := !errormessage ^ "\n" ^ string_of_int !errormessagecounter ^  str ^ "\n";
  if true then print_endline (str)
  else ()
  
  

let verifier_counter_reset_to n = verifier_counter := n


let rec string_with_seperator f li sep = 
  match li with 
  | [] -> ""
  | [x] -> f x 
  | x :: xs  -> f x ^ sep ^ string_with_seperator f xs sep



let current_source_file = ref ""

type state = int

type bin_op = GT | LT | EQ | GTEQ | LTEQ

type term = 
    | UNIT 
    | ANY
    | Nil
    | RES
    | Num of int
    | Var of string
    | Str of string
    (* | Pointer of string   *)
    | Plus of term * term 
    | Minus of term * term 
    | Rel of bin_op * term * term 
    | TTrue
    | TFalse
    | TAnd of term * term
    | TPower of term * term
    | TTimes of term * term
    | TDiv of term * term
    | TOr of term * term
    | TNot of term
    | TApp of string * term list
    | TCons of term * term
    | TList of term list
    | Member of term * term list

(*Arithmetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of term * term
          | Lt of term * term
          | GtEq of term * term
          | LtEq of term * term
          | Eq of term * term
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure
          | Exists of (string list) * pure 
          | Forall of (string list) * pure 

type signature = (string * (term list)) 


type core_value = term

type literal = string * (core_value list)

type event = 
  | Pos of literal 
  | Neg of literal  
  | NegTerm of term list | ANY 
  | Bag of interval * integratedSpec
  
and integratedSpec = ((pure * regularExpr) list)

and firstEle = event


and interval = (term * term)

and regularExpr = 
  | Bot 
  | Emp 
  | Singleton of event  
  | Disjunction of (regularExpr * regularExpr)
  | Conjunction of (regularExpr * regularExpr)
  | Concate of (regularExpr * regularExpr)
  | Kleene of regularExpr 


type futureCond = regularExpr 

let fc_default =  (Kleene (Singleton ANY))

type exitCode = int 

(* string list are the existential vars *)
type singleEffect = (string list * pure * regularExpr * futureCond * term * exitCode)
type effect = (singleEffect list)

type precondition = pure 

type summary = signature * precondition * effect

let (summaries: (summary list)ref) = ref []

let defaultSinglesEff = ([], TRUE, Emp, fc_default, Var "_" , 0 )

let verifier_get_A_freeVar term :string  =
  let prefix =
    match term with 
    | Var str -> "v"
    | _ -> "v"
  in
  let x = prefix ^ string_of_int (!verifier_counter) in
  incr verifier_counter;
  x

type core_lang = 
  | CSkip of state
  | CValue of core_value * state 
  | CLocal of string * state
  | CAssign of core_value * core_lang * state
  | CSeq of core_lang * core_lang 
  | CIfELse of pure * core_lang * core_lang * state
  | CFunCall of string * (core_value) list * state
  | CWhile of pure * core_lang * state
  | CBreak of state 
  | CContinue of state 
  | CLable of string * state 
  | CGoto of string * state 
  | CAssumeF of futureCond 

let rec flattenList lili = 
  match lili with 
  | [] -> []
  | x :: xs -> List.append x (flattenList xs) 

let cartesian_product li1 li2 = 
    flattenList (List.map li1 ~f:(fun l1 -> 
      List.map li2 ~f:(fun l2 -> (l1, l2))))


let rec existAux f (li:('a list)) (ele:'a) = 
  match li with 
  | [] ->  false 
  | x :: xs -> if f x ele then true else existAux f xs ele

let intersect f lst1 lst2 =
  List.filter ~f:(fun x -> existAux f lst2 x) lst1



let string_of_args pp args =
  match args with
  | [] -> "()"
  | _ ->
    let a = String.concat (List.map args ~f:pp) ~sep:", "  in
    Format.asprintf "(%s)" a

let string_of_bin_op op : string =
  match op with
  | GT -> ">"
  | LT -> "<"
  | EQ -> "="
  | GTEQ -> ">="
  | LTEQ -> "<="
let rec string_of_li f li sep =
  match li with 
  | [] -> ""
  | [x] -> f x 
  | x :: xs -> f x ^ sep ^ string_of_li f xs sep

let rec string_of_term t : string =
  match t with
  | RES -> "res"
  | Num i -> if i >=0 then string_of_int i else  "(" ^string_of_int i^ ")"
  | ANY -> "*"
  | UNIT -> "()"
  | Nil -> "nil"
  | TCons (a, b) -> Format.asprintf "%s::%s" (string_of_term a) (string_of_term b)
  | TTrue -> "true"
  | TFalse -> "false"
  | TNot a -> Format.asprintf "not(%s)" (string_of_term a)
  | TAnd (a, b) -> Format.asprintf "(%s&&%s)" (string_of_term a) (string_of_term b)
  | TOr (a, b) -> Format.asprintf "%s || %s" (string_of_term a) (string_of_term b)
  | Var str -> str
  | Str str -> "\'"^ str ^ "\'"

  (* | Pointer str -> "*" ^ str
*)
  | Rel (bop, t1, t2) ->
    "(" ^ string_of_term t1 ^ (match bop with | EQ -> "==" | _ -> string_of_bin_op bop) ^ string_of_term t2 ^ ")"
  | Plus (t1, t2) -> "(" ^string_of_term t1 ^ "+" ^ string_of_term t2^ ")"
  | Minus (t1, t2) -> "(" ^string_of_term t1 ^ "-" ^ string_of_term t2 ^ ")"
  | TPower (t1, t2) -> "(" ^string_of_term t1 ^ "^(" ^ string_of_term t2 ^ "))"
  | TTimes (t1, t2) -> "(" ^string_of_term t1 ^ "*" ^ string_of_term t2 ^ ")"
  | TDiv (t1, t2) -> "(" ^string_of_term t1 ^ "/" ^ string_of_term t2 ^ ")"
  | Member (t1, t2) -> string_of_li string_of_term (t1::t2) "." 
  | TApp (op, args) -> Format.asprintf "%s%s" op (string_of_args string_of_term args)
  | TList nLi ->
    let rec helper li =
      match li with
      | [] -> ""
      | [x] -> string_of_term x
      | x:: xs -> string_of_term x ^";"^ helper xs
    in "[" ^ helper nLi ^ "]"




let rec string_of_list_terms tL: string = 
  match tL with 
  | [] -> ""
  | [t] -> string_of_term t 
  | x :: xs ->  string_of_term x ^", "^ string_of_list_terms xs 


let rec string_of_pure (p:pure):string =   
  match p with
    TRUE -> "TRUE"  (*"⊤"*)
  | FALSE -> "⊥"
  | Gt (t1, t2) -> (string_of_term t1) ^ ">" ^ (string_of_term t2)
  | Lt (t1, t2) -> (string_of_term t1) ^ "<" ^ (string_of_term t2)
  | GtEq (t1, t2) -> (string_of_term t1) ^ ">=" ^ (string_of_term t2) (*"≥"*)
  | LtEq (t1, t2) -> (string_of_term t1) ^ "<=" ^ (string_of_term t2) (*"≤"*)
  | Eq (t1, t2) -> (string_of_term t1) ^ "=" ^ (string_of_term t2)
  | PureOr (p1, p2) -> "("^string_of_pure p1 ^ "∨" ^ string_of_pure p2^")"
  | PureAnd (p1, p2) -> string_of_pure p1 ^ "∧ " ^ string_of_pure p2 
  | Neg (Eq (t1, t2)) -> (string_of_term t1) ^ "!=" ^ (string_of_term t2)
  | Neg (Gt (t1, t2)) -> (string_of_term t1) ^ "<=" ^ (string_of_term t2)
  | Neg p -> "!(" ^ string_of_pure p^")"
  | Exists (str, p) -> "∃" ^ string_of_li (fun a -> a) str " "  ^ ". " ^ string_of_pure p 
  | Forall (str, p) -> "∀" ^ string_of_li (fun a -> a) str " "  ^ ". " ^ string_of_pure p 

let string_of_loc n = "@" ^ string_of_int n

let string_of_signature (str, args) = 
  str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) (args) "," ^ ")"

let string_of_interval ((i, j):interval) : string  = 
  "[" ^ string_of_term i ^ ".." ^ string_of_term j ^ ")"

let rec string_of_event (ev:event) : string = 
  match ev with 
  | Pos (str, args) -> str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) args "," ^ ")"
  | Neg (str, args) -> "!" ^ str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) args "," ^ ")"
  | ANY -> "_"
  | NegTerm args -> "!_" ^ "(" ^ string_with_seperator (fun a -> string_of_term a) args "," ^ ")"
  | Bag (interval, spec) -> string_of_interval interval ^ "(" ^ string_of_integratedSpec spec  ^ ")"

and string_of_integratedSpec (spec:integratedSpec) : string = 
  string_of_li (fun (p, re) -> string_of_pure p ^ "::" ^ string_of_regularExpr re ) spec " \/ "


and string_of_regularExpr re = 
  match re with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐 " 
  | Singleton (ev)  ->  string_of_event ev 
  | Concate (eff1, eff2) -> string_of_regularExpr eff1 ^ " · " ^ string_of_regularExpr eff2 
  | Disjunction (eff1, eff2) ->
      "((" ^ string_of_regularExpr eff1 ^ ") \\/ (" ^ string_of_regularExpr eff2 ^ "))"
  | Conjunction (eff1, eff2) ->
      "((" ^ string_of_regularExpr eff1 ^ ") /\\ (" ^ string_of_regularExpr eff2 ^ "))"
  
  | Kleene effIn          ->
      "(" ^ string_of_regularExpr effIn ^ ")^*"



let rec strict_compare_Term (term1:term) (term2:term) : bool =
  match (term1, term2) with
    (Var s1, Var s2) -> String.compare s1 s2 == 0
  | (Num n1, Num n2) -> n1 == n2
  | (Plus (tIn1, num1), Plus (tIn2, num2)) 
  | (TAnd (tIn1, num1), TAnd (tIn2, num2)) 
  | (TPower (tIn1, num1), TPower (tIn2, num2)) 
  | (TTimes (tIn1, num1), TTimes (tIn2, num2)) 
  | (TDiv (tIn1, num1), TDiv (tIn2, num2)) 
  | (TOr (tIn1, num1), TOr (tIn2, num2)) 
  | (TCons (tIn1, num1), TCons (tIn2, num2)) 
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> 
    strict_compare_Term tIn1 tIn2 && strict_compare_Term num1  num2
  | (TNot t1, TNot t2) -> strict_compare_Term t1 t2
  | (TList tLi1, TList tLi2) -> strict_compare_TermList tLi1 tLi2
  | (Member (t1, tLi1), Member (t2, tLi2)) -> strict_compare_TermList (t1::tLi1) (t2::tLi2)
  | (UNIT, UNIT) | (ANY, ANY) | (RES, RES) | (Nil, Nil) | (TTrue, TTrue) | (TFalse, TFalse) -> true
  | _ -> false

and  strict_compare_TermList li1 li2 = 
  match li1, li2 with 
  | [],  [] -> true 
  | x::xs, y::ys -> strict_compare_Term x y && strict_compare_TermList xs ys
  | _ , _ -> false 

let rec compareTermList tl1 tl2 : bool = 
  match tl1, tl2 with 
  | [], [] -> true 
  | (x:: xs, y:: ys) -> strict_compare_Term x y && compareTermList xs ys 
  | _ -> false 

let rec comparePure (pi1:pure) (pi2:pure):bool = 
  match (pi1 , pi2) with 
    (TRUE, TRUE) -> true
  | (FALSE, FALSE) -> true 
  | (Gt (t1, t11), Gt (t2, t22)) -> strict_compare_Term t1 t2 && strict_compare_Term t11  t22
  | (Lt (t1, t11), Lt (t2, t22)) -> strict_compare_Term t1 t2 && strict_compare_Term t11  t22
  | (GtEq (t1, t11), GtEq (t2, t22)) -> strict_compare_Term t1 t2 && strict_compare_Term t11  t22
  | (LtEq (t1, t11), LtEq (t2, t22)) -> strict_compare_Term t1 t2 && strict_compare_Term t11  t22
  | (Eq (t1, t11), Eq (t2, t22)) -> strict_compare_Term t1 t2 && strict_compare_Term t11  t22
  | (PureOr (p1, p2), PureOr (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (PureAnd (p1, p2), PureAnd (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (Neg p1, Neg p2) -> comparePure p1 p2
  | _ -> false


let normalize_terms (t:term) : term = 
  match t with 

  | Minus (Minus(_end, b), Minus(_end1, Plus(b1, inc))) -> 
    if strict_compare_Term _end _end1 && strict_compare_Term b b1 then inc 
    else t 

  | Minus(Plus((Var x),( Num n1)), Plus(Minus((Var x1),( Var y)), ( Num n2))) -> 
    if String.compare x x1 == 0 then 
      if (n2-n1) == 0 then ( Var y)
      else if n2-n1 > 0 then Minus(( Var y), ( Num (n2-n1)))
      else Plus(( Var y), ( Num (n2-n1)))
    else t

  
  | Minus (t1, t2) -> 
    if strict_compare_Term t1 t2 then (Num 0)
    else 

    (match t2 with
    | Minus (t21, t3) -> 
      if strict_compare_Term t1 t21 then t3 
      else t 
    | _ -> t )
    
  | _ -> t 


(*8*******************************)
open Z3

let counter : int ref = ref 0 ;;


let (historyTable: ((string * bool)list)ref) = ref [] ;;

let rec existInhistoryTable pi table= 
  match table with 
  | [] -> None
  | (x, b)::xs -> 
    if String.compare x (string_of_pure pi) == 0 then Some b 
    else existInhistoryTable pi  xs



let rec term_to_expr ctx : term -> Z3.Expr.expr = function
  | ((Num n))        -> Z3.Arithmetic.Real.mk_numeral_i ctx n
  | ((Var v))           -> Z3.Arithmetic.Real.mk_const_s ctx v
  | ((Nil))           -> Z3.Arithmetic.Real.mk_const_s ctx "nil"
  | ((RES))           -> Z3.Arithmetic.Real.mk_const_s ctx "ret"
  | (Member (a, b))   -> 
    let str = (string_of_term a ^ "_" ^ string_of_li (string_of_term) b "_") in 
    Z3.Arithmetic.Real.mk_const_s ctx str
  (*
  | Gen i          -> Z3.Arithmetic.Real.mk_const_s ctx ("t" ^ string_of_int i ^ "'")
  *)
  | Plus (t1, t2)  -> Z3.Arithmetic.mk_add ctx [ term_to_expr ctx t1; term_to_expr ctx t2 ]
  | Minus (t1, t2) -> Z3.Arithmetic.mk_sub ctx [ term_to_expr ctx t1; term_to_expr ctx t2 ]
  | _ -> Z3.Arithmetic.Real.mk_const_s ctx "nil"




let rec pi_to_expr ctx : pure -> Expr.expr = function
  | TRUE                -> Z3.Boolean.mk_true ctx
  | FALSE               -> Z3.Boolean.mk_false ctx
  | Gt (t1, t2) -> 
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      Z3.Arithmetic.mk_gt ctx t1 t2
  | GtEq (t1, t2) -> 
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      Z3.Arithmetic.mk_ge ctx t1 t2
  | Lt (t1, t2) -> 
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      Z3.Arithmetic.mk_lt ctx t1 t2
  | LtEq (t1, t2) -> 
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      Z3.Arithmetic.mk_le ctx t1 t2
  | Eq (t1, t2) -> 
      let newP = PureAnd (GtEq(t1, t2), LtEq(t1, t2)) in 
      pi_to_expr ctx newP
(*
  | Atomic (op, t1, t2) -> (
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      match op with
      | Eq -> Z3.Boolean.mk_eq ctx t1 t2
      | Lt -> Z3.Arithmetic.mk_lt ctx t1 t2
      | Le -> Z3.Arithmetic.mk_le ctx t1 t2
      | Gt -> Z3.Arithmetic.mk_gt ctx t1 t2
      | Ge -> Z3.Arithmetic.mk_ge ctx t1 t2)
      *)
  | PureAnd (pi1, pi2)      -> Z3.Boolean.mk_and ctx [ pi_to_expr ctx pi1; pi_to_expr ctx pi2 ]
  | PureOr (pi1, pi2)       -> Z3.Boolean.mk_or ctx [ pi_to_expr ctx pi1; pi_to_expr ctx pi2 ]
  (*| Imply (pi1, pi2)    -> Z3.Boolean.mk_implies ctx (pi_to_expr ctx pi1) (pi_to_expr ctx pi2)
  *)
  | Neg pi              -> Z3.Boolean.mk_not ctx (pi_to_expr ctx pi)
  | Exists (strs, p) -> 
    let body = pi_to_expr ctx p in 
    let int_sort = Arithmetic.Integer.mk_sort ctx in
    let xs = List.map ~f:(fun a -> Symbol.mk_string ctx a) strs in
    let quantifier = Z3.Quantifier.mk_exists ctx [int_sort] xs body None [] [] None None  in 
    let quantifier_expr = Quantifier.expr_of_quantifier quantifier in
    quantifier_expr
  | Forall (strs, p) -> 
    let body = pi_to_expr ctx p in 
    let int_sort = Arithmetic.Integer.mk_sort ctx in
    let xs = List.map ~f:(fun a -> Symbol.mk_string ctx a) strs in
    let quantifier = Z3.Quantifier.mk_forall ctx [int_sort] xs body None [] [] None None  in 
    let quantifier_expr = Quantifier.expr_of_quantifier quantifier in
    quantifier_expr

    ;;


let check pi =
  let cfg = [ ("model", "false"); ("proof", "false") ] in
  let ctx = mk_context cfg in
  let expr = pi_to_expr ctx pi in
  (* print_endline (Expr.to_string expr); *)
  let goal = Goal.mk_goal ctx true true false in
  (* print_endline (Goal.to_string goal); *)
  Goal.add goal [ expr ];
  let solver = Solver.mk_simple_solver ctx in
  List.iter ~f:(fun a -> Solver.add solver [ a ]) (Goal.get_formulas goal);
  let sat = Solver.check solver [] == Solver.SATISFIABLE in
  (* print_endline (Solver.to_string solver); *)
  sat

let askZ3 pi = 
  match existInhistoryTable pi !historyTable with 
  | Some b  -> b
  | None ->
  
  let _ = counter := !counter + 1 in 
  let re = check pi in 
  let ()= historyTable := (string_of_pure pi, re)::!historyTable in 
  
  re;;


let entailConstrains pi1 pi2 = 

  let sat = not (askZ3 (Neg (PureOr (Neg pi1, pi2)))) in
  
  (* print_string (string_of_bool (sat) ^ "\n"); *) 
  
  sat;;

let rec getUnification pi t l : pure option = 
  match pi with 
  | Eq (t1, t2 ) -> 
    if strict_compare_Term t1 t then Some (Eq(t2, Var l)) 
    else if strict_compare_Term t2 t then Some (Eq(t1, Var l)) 
    else None 
  | PureAnd (p1, p2) -> 
    (match getUnification p1 t l with 
    | None -> getUnification p2 t l 
    | Some resdue -> Some resdue
    )
  | _ -> None 

let checkPreCondition pi1 pi2 = 

  match pi2 with 
  | Exists (l::_, Eq(Var t1, Var t2)) -> 
    debug_printCFunCall (string_of_pure pi1 ^" -> " ^ string_of_pure pi2 ^" == ");
    let target = if String.compare l t1 == 0 then Var t2 
                 else Var t1 in 
    getUnification pi1 target l 
  | TRUE -> Some TRUE 
    
  | _ -> 
    debug_printCFunCall (string_of_pure pi1 ^" -> " ^ string_of_pure pi2 ^" == ");

    if entailConstrains pi1 pi2 then Some TRUE 
    else None 

let rec actual_formal_mappings (arctul_args:term list) (formal_args:term list) : ((term * term) list) = 
  match arctul_args, formal_args with 
  | [], [] -> [] 
  | x ::xs , y::ys -> (x, y) :: (actual_formal_mappings xs ys)
  | _, _ -> 
    debug_print ("there is a mismatch of actual and formal arguments!!!");
    debug_print (string_of_list_terms arctul_args); 
    debug_print (string_of_list_terms formal_args); 
    []

let string_of_mappings (mappings: ((term * term) list)) : string = 
  (string_of_li (fun(actual, formal) -> string_of_term formal ^ " -> " ^ string_of_term actual )  mappings ", ")

let compare_event (ev1:event) (ev2:event) : bool = 
  if String.compare (string_of_event ev1) (string_of_event ev2) == 0 then true 
  else false  


(*8*******************************)


let rec nullable (eff:regularExpr) : bool = 
  match eff with 
  | Bot            -> false 
  | Emp            -> true 
  | Singleton _    -> false
  | Concate (eff1, eff2) -> nullable eff1 && nullable eff2  
  | Disjunction (eff1, eff2) -> nullable eff1 || nullable eff2  
  | Conjunction (eff1, eff2) -> nullable eff1 && nullable eff2  
  | Kleene _       -> true


let rec re_fst re : firstEle list = 
  match re with 
  | Emp 
  | Bot -> [] 
  | Singleton x -> [x]
  | Concate (eff1, eff2) -> 
    let temp = (re_fst eff1) in 
    if nullable eff1 then temp @ (re_fst eff2  )
    else temp
  | Disjunction (eff1, eff2) -> (re_fst eff1) @ (re_fst eff2  )
  | Conjunction (eff1, eff2) -> 
    intersect compare_event (re_fst eff1) (re_fst eff2  )

  | Kleene re1 -> re_fst re1 

let compareTermWithPure (p:pure) (t1:term) (t2:term) : bool = 
  let (equlity:pure) = Eq (t1, t2)  in
  if entailConstrains p equlity then true 
  else false
  ;;

let compareTermListWithPure (p:pure) (t1:term list) (t2:term list) : bool = 
  let pairs = actual_formal_mappings t1 t2 in  
  let (equlity:pure) = List.fold_left ~f:(fun acc (t1, t2) ->  PureAnd (acc, Eq (t1, t2))) ~init:TRUE pairs  in
  (*debug_print (string_of_pure p ^ " <: " ^ string_of_pure equlity );  *) 
  if entailConstrains p equlity then (true )
  else (false)
  ;;

let has_overlap compareFun list1 list2 =
  List.exists ~f:(fun x -> existAux compareFun list2 x) list1

(* list1 is a super set of list2 *)
let superset list1 list2 p  = 
  List.for_all ~f:(fun x -> existAux (fun a b -> compareTermWithPure p a b) list1 x) list2

(*  this is to compute the derivative of ev-1(evTarget) *)

let rec normalize_pure (pi:pure) : pure = 
  match pi with 
  | TRUE 
  | FALSE -> pi
  | LtEq ((Num n), (Var v)) -> GtEq ((Var v), (Num n))
  | Lt ((Num n), (Var v)) -> Gt ((Var v), (Num n))
  | Gt ((Num n), (Var v)) -> Lt ((Var v), (Num n))

  | Gt (leftHandside,( Num 0)) -> 
    (match normalize_terms leftHandside with
    | Minus(t1, t2) -> Gt (t1, t2)
    | Plus(t1, ( Num n)) -> Gt (t1,  ( Num (-1 * n)))
    | t -> Gt(t, ( Num 0))
    )
  | LtEq (Minus(t1, t2),( Num 0)) -> LtEq (t1, t2)
  | Gt (Minus((Num n1),( Var v1)),( Num n2)) -> Lt((Var v1),  (Num(n1-n2)))
  | Gt (t1, t2) -> Gt (normalize_terms t1, normalize_terms t2)
  | Lt (t1, t2) -> Lt (normalize_terms t1, normalize_terms t2)
  | GtEq (t1, t2) -> GtEq (normalize_terms t1, normalize_terms t2)
  | LtEq (Minus((Var x),( Num n1)), Minus(Minus((Var x1),( Var y)), ( Num n2))) -> 
    if String.compare x x1 == 0 then  LtEq((Var y), ( Num (n2-n1)))
    else LtEq (normalize_terms (Minus((Var x),( Num n1))), normalize_terms (Minus(Minus((Var x1),( Var y)), ( Num n2))))

  | LtEq (t1, t2) -> LtEq (normalize_terms t1, normalize_terms t2)
  | Eq (t1, t2) -> Eq (normalize_terms t1, normalize_terms t2)
  | PureAnd (pi1,pi2) -> 
    let p1 = normalize_pure pi1 in 
    let p2 = normalize_pure pi2 in 
    (match p1, p2 with 
    | TRUE, _ -> p2
    | _, TRUE -> p1
    | FALSE, _ 
    | _, FALSE -> FALSE
    | _ ->
      if comparePure p1 p2 then p1
      else PureAnd (p1, p2)
    )

  | Neg (TRUE) -> FALSE
  | Neg (Neg(p)) -> p
  | Neg (Gt (t1, t2)) -> LtEq (t1, t2)
  | Neg (Lt (t1, t2)) -> GtEq (t1, t2)
  | Neg (GtEq (t1, t2)) -> Lt (t1, t2)
  | Neg (LtEq (t1, t2)) -> Gt (t1, t2)
  | Neg piN -> Neg (normalize_pure piN)
  | PureOr (pi1,pi2) -> PureAnd (normalize_pure pi1, normalize_pure pi2)
  | Exists (str, p) -> Exists (str, normalize_pure p)
  | Forall (str, p) -> Forall (str, normalize_pure p)

let rec normalize_es (eff:regularExpr) : regularExpr = 
  match eff with 
  | Disjunction(es1, es2) -> 
    let es1 = normalize_es es1 in 
    let es2 = normalize_es es2 in 
    (match (es1, es2) with 
    | (Emp, Emp) -> Emp
    | (Emp, _) -> if nullable es2 then es2 else (Disjunction (es2, es1))
    | (Bot, es) -> normalize_es es 
    | (es, Bot) -> normalize_es es 
    | _ -> (Disjunction (es1, es2))
    )
  | Conjunction(es1, es2) -> 
    let es1 = normalize_es es1 in 
    let es2 = normalize_es es2 in 
    (match (es1, es2) with 
    | (Emp, Emp) -> Emp
    | (Bot, _) 
    | (_, Bot) -> Bot
    | (es, Kleene (Singleton ANY)) 
    | (Kleene (Singleton ANY), es) -> es
    | _ -> (Conjunction (es1, es2))
    )
  | Concate (es1, es2) -> 
    let es1 = normalize_es es1 in 
    let es2 = normalize_es es2 in 
    (match (es1, es2) with 
    | (Emp, _) -> normalize_es es2
    | (_, Emp) -> normalize_es es1
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Concate (es11, es12), es3) -> (Concate (es11, normalize_es (Concate (es12, es3))))
    | _ -> (Concate (es1, es2))
    )
  | Kleene effIn -> 
    let effIn' = normalize_es effIn in 
    Kleene (effIn')

  | _ -> eff 

let string_of_loc n = "@" ^ string_of_int n 

let rec string_of_fc (fc:futureCond) : string = string_of_regularExpr fc 
  (* string_with_seperator (fun a -> "("^string_of_regularExpr a ^")") fc " /\\ " *)
  

let rec string_of_core_lang (e:core_lang) :string =
  match e with
  | CSkip state -> "skip"  ^ string_of_loc state 
  | CValue (v, state) -> string_of_term v ^ string_of_loc state 
  | CAssign (v, e, state) -> Format.sprintf "%s=%s " (string_of_term v) (string_of_core_lang e) ^ string_of_loc state 
  | CIfELse (pi, t, e, state) -> Format.sprintf "if (%s) then %s else (%s)" (string_of_pure pi)  (string_of_core_lang t) (string_of_core_lang e) ^ string_of_loc state
  | CFunCall (f, xs, state) -> Format.sprintf "%s(%s)" f (List.map ~f:string_of_term xs |> String.concat ~sep:",") ^ string_of_loc state 
  | CLocal (str, state) -> Format.sprintf "local %s " str ^ string_of_loc state 
  | CSeq (e1, e2) -> Format.sprintf "%s\n%s" (string_of_core_lang e1) (string_of_core_lang e2) 
  | CWhile (pi, e, state) -> Format.sprintf "while (%s)\n{%s}" (string_of_pure pi) (string_of_core_lang e) ^ string_of_loc state 
  | CBreak  state ->  "Break" ^ string_of_loc state
  | CContinue state -> "Continue" ^ string_of_loc state
  | CLable (str, state) ->  str ^ ": " ^ string_of_loc state
  | CGoto (str, state) -> "goto " ^ str ^ " " ^ string_of_loc state
  | CAssumeF (fc) -> "AssumeF" ^ string_of_fc fc



let rec removeAny fcIn = 
  if List.length fcIn <= 1 then fcIn 
  else
    let temp = 
    List.filter ~f:(fun a -> match a with | Kleene (Singleton ANY) -> false | _ -> true) fcIn in 
    if List.length temp == 0 then [Kleene (Singleton ANY)]
    else temp

let compare_interval (i1, i2) (i3, i4) : bool = 
  strict_compare_Term i1 i3 && strict_compare_Term i2 i4 

let normalize_bag_in_fc (fc:futureCond) : futureCond =  fc 
  (*
  match fc with 
  | Singleton (Bag (i1, spec1)) :: Singleton (Bag (i2, spec2)) :: rest -> 
    if compare_interval i1 i2 then 
      let product = cartesian_product spec1 spec2 in 
      let combine = List.fold_left (fun acc (s1, s2) -> 
        let (p1, re1) = s1 in 
        let (p2, re2) = s2 in 
        if entailConstrains (PureAnd(p1, p2)) FALSE then [] 
        else 

      ) ~init:[] product in 
      combine @ rest
    else fc 
  | _ -> fc
  ;;
let normalize_fc (fc:futureCond) : futureCond = 
  let rec existBot (fcIn:futureCond) : bool =
    match fcIn with 
    | [] -> false 
    | Bot :: _ -> true 
    | _ :: xs -> existBot xs 
  in 
  (*debug_print ("original_fc: " ^ string_of_fc fc ); *) 
  let fc' = (List.map ~f:normalize_es fc) in 
  (*debug_print ("normalized fc: " ^ string_of_fc fc' ); *)
  if existBot fc' then [Bot] 
  else 
    let afterRemoveAny =  removeAny fc' in 
    normalize_bag_in_fc afterRemoveAny

  *)

  let normalize_fc (fc:futureCond) : futureCond = normalize_es fc 



let rec normalize_effect (summary:effect)  : effect = 
  let normalize_effect_a_pair (exs, p, re, fc, r, exitCode) = 
    let p' = normalize_pure p in
    if entailConstrains p' FALSE then []
    else [(exs, normalize_pure p, normalize_es re, normalize_es fc, r, exitCode)] in 
  match summary with 
  | [] -> []
  | x :: xs -> (normalize_effect_a_pair x)  @  (normalize_effect xs)

let string_of_exs exs = string_with_seperator (fun a -> a) exs " "

let string_of_single_effect (exs, p, re, fc, r, exitCode) = 
  (if List.length exs == 0 then ":" else "∃" ^  string_of_exs exs ^  ": ") ^ 
  string_of_pure p ^ " ; " ^ 
  string_of_regularExpr re ^ " ; " ^ 
  string_of_fc fc   ^ " ; " ^ 
  string_of_term r  ^  
  (if exitCode < -1 then "; ERROR PATH! "  else "" )
  (* ^ string_of_int exitCode  *)

let intersectionTwoInterval (p:pure) (intervalTarget:interval) (interval:interval) : (interval * interval list) = 
  let (iTargetLow, iTargetHigh) =  intervalTarget in
  let (iLow, iHigh) =  interval in
  (* Target - Ev *)
  (* [1-3] - [2-2] *)
  if entailConstrains p (PureAnd(GtEq(iHigh, iTargetHigh), LtEq(iLow, iTargetLow))) 
  then intervalTarget, []
  else if strict_compare_Term iTargetLow iLow && entailConstrains p (Gt(iTargetHigh, iHigh)) 
  then interval, [(Plus(iTargetHigh, Num 1), iHigh)]
  else if strict_compare_Term iTargetHigh iHigh && entailConstrains p (Lt(iTargetLow, iLow)) 
  then interval, [(iTargetLow, Minus(iLow, Num 1))]
  else interval, [(Plus(iTargetHigh, Num 1), iHigh); (iTargetLow, Minus(iLow, Num 1))]
  


let rec derivative (p:pure) (ev:firstEle) (re:regularExpr) : regularExpr = 
  match re with 
  | Kleene (Singleton ANY) -> re
  | Emp | Bot -> Bot 
  | Singleton evIn -> 
  
    let deriveEv = derivativeEvent p ev evIn  in 
    debug_derivative("\nderivative: " ^ string_of_event evIn ^ " - " ^ string_of_event ev ^ " ~~> "  ^ string_of_fc deriveEv); 

    deriveEv
   
  | Concate(re1, re2) -> 
    let resRe1 = Concate (derivative p ev re1, re2) in 
    if nullable re1 then Disjunction(resRe1, derivative p ev re2)
    else resRe1
  | Disjunction(re1, re2) -> Disjunction(derivative p ev re1, derivative p ev re2) 
  | Conjunction(re1, re2) -> Conjunction(derivative p ev re1, derivative p ev re2) 
  | Kleene reIn -> Concate (derivative p ev reIn, re)

and derivativeIntegratedSpec (contextP:pure) (spec:integratedSpec) (specTarget:integratedSpec) : integratedSpec = 
  List.fold_left ~f:(fun acc (pureT, esT) -> 
    
    let rec helper li = 
      match li with 
      | [] -> acc@[(pureT, esT)]
      | (p, es)::xs -> 
        if entailConstrains (PureAnd(p, pureT)) FALSE then helper xs 
        else 
          let (subtraction:futureCond) =  (trace_subtraction_helper contextP esT es) in 
          acc@[(pureT, subtraction)]
          
    in helper spec 

  ) ~init:[] specTarget

and derivativeEvent (p:pure) (ev:firstEle) (evTarget:firstEle) : futureCond = 
match ev with 
| Pos (str, args) -> 
  (match evTarget with 
  | ANY -> Emp  
  | Pos (strTarget, argsTarget) ->  
    if String.compare str strTarget == 0 && List.length args == List.length argsTarget then 
      if compareTermListWithPure p args argsTarget then Emp 
      else Bot
    else Bot
  | Neg (strTarget, argsTarget) -> 
    if String.compare str strTarget != 0 || List.length args != List.length argsTarget then Emp 
    else if compareTermListWithPure p args argsTarget == false then Emp 
    else Bot  
    
  | NegTerm (argsTarget:term list) -> 
    if has_overlap (compareTermWithPure p) args argsTarget then Bot 
    else Emp 
  | Bag _ -> 
    debug_print ("I am not too sure derivativeEvent 1"); 
    Singleton evTarget
  )
| NegTerm (args) -> 
  (match evTarget with 
  | ANY -> Emp  
  | Pos (_, argsTarget) ->  
    if has_overlap (compareTermWithPure p) args argsTarget then Bot 
    else Emp 
  | NegTerm (argsTarget:term list)  -> 
    (* !_(li1) <: !_(li2) iff li1 >= li2 *)
    if superset args argsTarget p then  Emp 
    else Bot
  | Neg (strTarget, argsTarget) -> Bot
    
  | Bag _ -> 
    debug_print ("I am not too sure derivativeEvent 2"); 
    Singleton evTarget
  )

| Bag (interval, spec)  -> 
  (match evTarget with 
  | ANY -> Emp 
  | Bag (intervalTarget, specTarget) -> 
    let commonArr, outstandingArr = intersectionTwoInterval p intervalTarget interval in 

    debug_derivative ("commonArr     : " ^ string_of_interval commonArr); 
    debug_derivative ("outstandingArr: " ^ string_of_li string_of_interval outstandingArr ", "); 

    debug_derivative(">>>>>>>>>>");
    debug_derivative ("spec      : " ^ string_of_integratedSpec spec); 
    debug_derivative ("specTarget: " ^ string_of_integratedSpec specTarget); 
    let deriIntegrated = derivativeIntegratedSpec p spec specTarget in 
    debug_derivative ("deriInt   : " ^ string_of_integratedSpec deriIntegrated); 
    if List.for_all ~f:(fun (p, es) -> match es with | Emp -> true | _ -> false) deriIntegrated then Emp 
    else if String.compare (string_of_event ((Bag (commonArr, deriIntegrated)) )) (string_of_event ev) == 0 then Bot 
    else  
      Singleton (Bag (commonArr, deriIntegrated)) 
    
  | Neg (strTarget, argsTarget) -> Bot

  | _ -> Bot
  )
| ANY  -> 
  (match evTarget with 
  | ANY -> Emp 
  | _ -> Bot
  )
| Neg (str, args) -> 
  (match evTarget with 
  | ANY -> Emp 
  | _ -> Bot
  )




and trace_subtraction_helper (p:pure) (fc: regularExpr) (es:regularExpr) : futureCond = 
  let fc = normalize_es fc in 
  let es = normalize_es es in 
  match es with 
  | Emp -> fc 
  | Bot -> Bot 
  | _ -> 
    let fst = re_fst es in 
    let res = 
      List.fold_left ~f:(fun acc ev -> 
      let derive1 = derivative p ev fc in 
      let derive2 = derivative p ev es in  
      let temp = trace_subtraction_helper p derive1 derive2 in
      Disjunction (acc, temp)) ~init:Bot fst  in 
    let res = normalize_es res  in 
    (* debug_print ("trace_subtraction_single: " ^ string_of_regularExpr fc ^ " - " ^ string_of_regularExpr es ^ " ~~> "^ string_of_regularExpr res);
     *)
    res


let trace_subtraction (lhsP:pure) (rhsP:pure) (fc: futureCond) (es:regularExpr) (fp:int): futureCond =
  debug_printTraceSubtraction ("======="); 
  debug_printTraceSubtraction (string_of_pure lhsP ^ " - " ^ string_of_pure rhsP);
  debug_printTraceSubtraction (string_of_fc fc ^ " - " ^ string_of_regularExpr es);
  
  let res = normalize_es (

    let p =  (PureAnd(lhsP, rhsP)) in 
    let single_res = trace_subtraction_helper p fc es in 
    (match single_res with 
    | Bot -> 
      error_message ("\nThe future condition is violated here at line " ^ string_of_int fp ^ "\n  Future condition is = " ^ string_of_regularExpr fc ^ "\n  Trace subtracted = " ^ string_of_regularExpr es ^ "\n  Pure =  " ^ string_of_pure p);  
    | _ -> ()
    ); 
    
    single_res

  ) in 
  debug_printTraceSubtraction ("res = " ^ string_of_fc res ^ "\n");
  res



let rec string_of_effect (summary:effect) = 

  let string_of_a_pair (exs, p, re, fc, r, exitCode) = string_of_single_effect (exs, p, re, fc, r, exitCode) in 

  match summary with 
  | [] -> ""
  | [x] -> "(" ^ string_of_a_pair x ^ ")"
  | x :: xs -> "(" ^ string_of_a_pair x  ^ ") \\/ \n    " ^ string_of_effect xs 



let rec reverse li = 
  match li with 
  | [] -> [] 
  | x :: xs  -> reverse(xs) @ [x]

let string_of_summary ((signature, preC, disjRE):summary) = 
  "/*@ " ^ string_of_signature signature ^ " = \n" ^ "REQ " ^ string_of_pure preC ^ "\nENS " ^string_of_effect disjRE ^ " @*/\n"

let rec string_of_summaries li = 
  match li with 
  | [] -> "" 
  | x :: xs  -> 
    string_of_summary x ^ 
    string_of_summaries xs 

let rec find_from_mapping_list t (actual_formal_mappings:((term*term)list)) : term option  = 
  match actual_formal_mappings with 
  | [] -> None 
  | (arctual, formal)::xs  -> 
    if strict_compare_Term formal t then Some arctual
    else find_from_mapping_list t xs 
    

let substitute_term_aux (t:term) (actual_formal_mappings:((term*term)list)): term = 
  match find_from_mapping_list t actual_formal_mappings with 
  | None -> t 
  | Some t' -> t'



let rec substitute_term (t:term) (actual_formal_mappings:((term*term)list)): term = 
  match t with
  | Var _ -> substitute_term_aux t actual_formal_mappings

  | TCons (a, b) -> 
    TCons (substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)
  | TNot a -> TNot (substitute_term a actual_formal_mappings)

  | TAnd (a, b) -> 
    TAnd (substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)

  | TOr (a, b) -> 
    TOr (substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)
  | Rel (bop, a, b) ->
    Rel (bop, substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)

  | Plus (a, b) ->  Plus (substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)
  | Minus (a, b) ->  Minus (substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)
  | TPower (a, b) ->  TPower (substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)
  | TTimes (a, b) ->  TTimes (substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)
  | TDiv (a, b) ->  TDiv(substitute_term a actual_formal_mappings, substitute_term b actual_formal_mappings)
  | Member (a, b) -> 
    (match find_from_mapping_list t actual_formal_mappings with 
    | Some t' -> t' 
    | None -> 
      let b' = List.map b ~f:(fun c -> substitute_term c actual_formal_mappings) in 
      Member (substitute_term a actual_formal_mappings, b'))
  | TApp (op, args) -> 
    let args' =List.map args ~f:(fun a -> substitute_term a actual_formal_mappings) in 
    TApp (op, args')
  | TList nLi -> 
    let nLi' =List.map nLi ~f:(fun a -> substitute_term a actual_formal_mappings) in 
    TList nLi'
 

  | _ -> t 



let substitute_term_pair (t1, t2) actual_formal_mappings = 
  (substitute_term t1 actual_formal_mappings, substitute_term t2 actual_formal_mappings)


let rec substitute_pure (p:pure) (actual_formal_mappings:((term*term)list)): pure = 
  match p with 
  | Gt (a, b) -> 
    let (a', b') = substitute_term_pair (a, b) actual_formal_mappings in 
     Gt (a', b')

  | Lt (a, b) -> 
    let (a', b') = substitute_term_pair (a, b) actual_formal_mappings in 
    Lt (a', b')

  | GtEq (a, b) -> 
    let (a', b') = substitute_term_pair (a, b) actual_formal_mappings in 
    GtEq (a', b')

  | LtEq (a, b) -> 
    let (a', b') = substitute_term_pair (a, b) actual_formal_mappings in 
    LtEq (a', b')

  | Eq (a, b) -> 
    let (a', b') = substitute_term_pair (a, b) actual_formal_mappings in 
    if strict_compare_Term a' b' then TRUE
    else
    Eq (a', b')

  | PureOr (p1, p2) -> 
    let p1' = substitute_pure p1 actual_formal_mappings in 
    let p2' = substitute_pure p2 actual_formal_mappings in 
    PureOr (p1', p2') 
  | PureAnd (p1, p2)  -> 
    let p1' = substitute_pure p1 actual_formal_mappings in 
    let p2' = substitute_pure p2 actual_formal_mappings in 
    PureAnd (p1', p2') 

  | Neg pIn -> 
    let pIn' = substitute_pure pIn actual_formal_mappings in 
    Neg pIn'

  | _ -> p 

let rec substitute_event (ev:event) (actual_formal_mappings:((term*term)list)): event = 
  match ev with 
  | Pos (str, args) -> Pos (str, List.map ~f:(fun a -> substitute_term a actual_formal_mappings) args)
  | Neg (str, args) -> Neg (str, List.map ~f:(fun a -> substitute_term a actual_formal_mappings) args)
  
  | ANY -> ANY
  | NegTerm (args) -> NegTerm (List.map ~f:(fun a -> substitute_term a actual_formal_mappings) args)
  | Bag ((b1, b2), li) -> 
    let b1' = substitute_term b1 actual_formal_mappings in 
    let b2' = substitute_term b2 actual_formal_mappings in 
    let li' = List.map ~f:(fun (p, es)-> substitute_pure p actual_formal_mappings, substitute_RE es actual_formal_mappings) li  
    in 
    Bag((b1', b2'), li')

and substitute_RE (re:regularExpr) (actual_formal_mappings:((term*term)list)): regularExpr = 
  match re with
  | Singleton ev  -> Singleton (substitute_event ev actual_formal_mappings) 
  | Concate (eff1, eff2) ->  
    Concate(substitute_RE eff1 actual_formal_mappings, substitute_RE eff2 actual_formal_mappings)
  | Disjunction (eff1, eff2) ->
    Disjunction(substitute_RE eff1 actual_formal_mappings, substitute_RE eff2 actual_formal_mappings)
     
  | Kleene effIn -> Kleene (substitute_RE effIn actual_formal_mappings)
  (*
  | Bag((b1, b2), li) -> 
    let b1' = substitute_term b1 actual_formal_mappings in 
    let b2' = substitute_term b2 actual_formal_mappings in 
    let li' = List.map ~f:(fun (p, es)-> substitute_pure p actual_formal_mappings, substitute_RE es actual_formal_mappings) li  
    in 
    Bag((b1', b2'), li')
    *)
  | _ -> re

let rec substitute_FC (fc:futureCond) (actual_formal_mappings:((term*term)list)): futureCond = substitute_RE fc actual_formal_mappings

let substitute_single_effect (spec:singleEffect) (actual_formal_mappings:((term*term)list)): singleEffect =
  let (exs, p, re, fc, r, exitCode) = spec in 
  let p' = substitute_pure p actual_formal_mappings in 
  let re' = substitute_RE re actual_formal_mappings in 
  let fc' = substitute_FC fc actual_formal_mappings in 
  let r' = substitute_term r actual_formal_mappings in 
  (exs, p', re', fc', r', exitCode)

let substitute_effect (spec:effect) (actual_formal_mappings:((term*term)list)): effect =
  List.map ~f:(fun single -> 
    substitute_single_effect single actual_formal_mappings) 
  spec


let rec getAllTermsFromEvent (ev:event) : term list = 
  match ev with 
  | Pos (_, args)
  | Neg (_, args) 
  | NegTerm args -> args 
  | ANY -> []
  | Bag ((b1, b2), li) -> 
  let tLi = List.fold_left ~f:(fun acc (p, es) -> acc @ getAllTermsFromRE es) ~init:[] li in 
  tLi


and getAllTermsFromRE re : term list =    
  match re with 
  | Singleton ev  -> 
    let tep = getAllTermsFromEvent ev in 
    tep 
  | Concate (eff1, eff2) 
  | Conjunction (eff1, eff2)
  | Disjunction (eff1, eff2) -> getAllTermsFromRE eff1 @ getAllTermsFromRE eff2
  | Kleene reIn  -> getAllTermsFromRE reIn  
  | Emp | Bot  -> [] 
  

let getAllTermsFromFC (fc:futureCond) : term list = getAllTermsFromRE fc

let rec getAllTermsFromPure (p:pure) : term list =
  match p with 
  | Gt (t1, t2)
  | Lt (t1, t2)
  | GtEq (t1, t2)
  | LtEq (t1, t2)
  | Eq (t1, t2) -> [t1; t2]
  | PureOr (p1, p2)
  | PureAnd (p1, p2) -> getAllTermsFromPure p1 @ getAllTermsFromPure p2
  | Neg pIn -> getAllTermsFromPure pIn
  | _ -> []
  ;;
    

let rec findATermEqualToX (p:pure) (x:string) : term list = 
  let aux (t:term) : term list =
    match t with 
    | Member _ | Var _ -> [t]
    | _ -> []
  in 
  match p with
  | Eq(t1, t2) -> 
    if strict_compare_Term t1 t2 then []
    else if strict_compare_Term t1 (Var x) then aux t2 
    else if strict_compare_Term t2 (Var x) then aux t1
    else []
  | PureAnd (p1, p2) -> findATermEqualToX p1 x @ findATermEqualToX p2 x
  | _ -> 
  []

let stringHasNum str =
  let found = ref false in
  String.iter ~f:(fun c -> if Char.is_digit c then found := true) str;
  !found

let rec findReplaceableVar exs p : (string * term) option = 
  match exs with 
  | [] -> None 
  | x :: xs -> 

    if stringHasNum x == false then findReplaceableVar xs p  
    else 
      let allTermEqualToX = findATermEqualToX p x in 
      (*
      debug_postprocess ("\nallTermEqualToX:\n" ^ x ^ "\n"^ string_of_list_terms allTermEqualToX);
      debug_postprocess (string_of_pure p);
      *)
      (match allTermEqualToX with 
      | [] -> findReplaceableVar xs p 
      | y::_ -> Some (x, y)
      )

;;

let rec removeIntermediateResHelper exs tobereplacedList = 
  match exs with
  | [] -> [] 
  | x :: xs -> 
    if existAux (fun a b -> if String.compare a b == 0 then true else false) tobereplacedList x 
    then removeIntermediateResHelper xs tobereplacedList
    else x:: removeIntermediateResHelper xs tobereplacedList





let rec removeIntermediateRes  (exs, p, re, fc, r, exitCode) : singleEffect = 
  match findReplaceableVar exs p with 
  | None -> (exs, p, re, fc, r, exitCode)
  | Some (tobereplaced, (ex:term)) -> 
    debug_postprocess ("\n========\nex: " ^ string_of_term ex);
    debug_postprocess ("tobereplacedList: " ^ tobereplaced );
    let mappings =  [(ex, Var tobereplaced)] in  
    let exs' = removeIntermediateResHelper exs [tobereplaced] in 
    let temp = (exs', substitute_pure p mappings, substitute_RE re mappings, substitute_FC fc mappings, substitute_term r mappings, exitCode)  in 

    debug_postprocess ("temp: " ^ string_of_effect [temp]);

    removeIntermediateRes   temp

  ;;

let strict_compare_Term_Forall_Exist (t1:term) (t2:term) : bool = 
  match t1, t2 with 
  | (Member (t1New, _), t2 ) 
  | (t2,  Member (t1New, _) ) ->  strict_compare_Term t1New t2 
  | _ , _ -> strict_compare_Term t1 t2 


let rec existTermInForall (fc_Vars:term list) (forallVar:term list) : bool = 
  match fc_Vars with 
  | [] -> false 
  | x :: xs  -> if existAux strict_compare_Term_Forall_Exist forallVar x then true else existTermInForall xs forallVar


let removeUnusedExs ((a, b, c, d, r, ec):singleEffect) : singleEffect =

  let allTerms = getAllTermsFromPure b @ getAllTermsFromRE c @ getAllTermsFromFC d @ [r] in 
  let allTerms = List.map ~f:(fun t -> match t with | Member(tIn, _)-> tIn | _ -> t)  allTerms in 

  let a' = List.filter ~f:(fun ex -> existAux strict_compare_Term allTerms (Var ex)) a in 
  (a', b, c, d, r, ec)

  
let postProcess  (eff:effect) : effect = 
  let rec helper (eff:effect) : effect = 
    match eff with 
    | [] -> []
    | single :: xs -> 
      removeUnusedExs (removeIntermediateRes single) :: helper xs
  in 
  normalize_effect (helper eff) 

let remove_duplicates f lst =
  let rec aux seen = function
    | [] -> List.rev seen
    | hd::tl ->
      if List.exists ~f:(fun x -> f x hd) seen then aux seen tl
      else aux (hd::seen) tl
  in
  aux [] lst


let rec notExistTermInForall (fc_Vars:term list) (forallVar:term list) : bool = 
  match fc_Vars with 
  | [] -> false 
  | x :: xs  -> if existAux strict_compare_Term_Forall_Exist forallVar x  == false then true else notExistTermInForall xs forallVar


(*
let decompositeFCByForallExists (fc:futureCond) (forallVar:term list): (futureCond * futureCond) = 
  List.fold_left ~f:(fun (fcInputOutput, fcExists) fcCurrent -> 
    let fc_Vars = remove_duplicates strict_compare_Term (getAllTermsFromFC [fcCurrent]) in 
    match fc_Vars with 
    | [] -> (fcInputOutput@[fcCurrent], fcExists)
    | fc_Vars ->  
      let fcInputOutput' = if existTermInForall fc_Vars forallVar then fcInputOutput@[fcCurrent] else fcInputOutput in 
      let fcExists' = if notExistTermInForall fc_Vars forallVar then fcExists@[fcCurrent] else fcExists in 
      (fcInputOutput', fcExists')
  ) ~init:([], []) fc
  ;;

let checkPostConditionError (eff:effect) (formalArgs:term list) (fp:int): effect = 
  let aux ((exs, p, re, fc, r, exitCode):singleEffect) : singleEffect option = 
    match fc with 
    | Bot -> (* if already error, then no need to check *) Some (exs, p, re, fc, r, exitCode)
    | _ -> 
      let inputOutputTerms = r::formalArgs in 
      let fcForall, fcExists = decompositeFCByForallExists fc inputOutputTerms in 
      debug_checkPostConditionError("fcForall = " ^ string_of_fc fcForall);
      debug_checkPostConditionError("fcExists = " ^ string_of_fc fcExists); 
      (if nullableFC fcExists 
      then ()
      else 
        error_message ("\nThe future condition is violated here at line " ^ string_of_int fp ^ "\n  Future condition is = " ^ string_of_fc fcExists ^ "\n  Trace subtracted = 𝝐 " ^ "\n  Pure =  " ^ string_of_pure p));  

      if List.length fcForall == 0 then None 
      else Some (exs, p, re, fcForall, r, exitCode)
  in 
  let rec helper (acc:effect) (effSingleLi:effect) = 
    match effSingleLi with 
    | [] -> acc 
    | x :: xs  -> 
      (match aux x with 
      | None -> helper acc xs
      | Some effSingle -> helper (acc@[effSingle]) xs
      )
  in 
  match helper [] eff with 
  | [] -> [defaultSinglesEff] 
  | eff -> eff
*)



let rec removePure (p:pure) (pToRemover:pure) : pure =
  match p with 
  | PureAnd (p1, p2) -> PureAnd (removePure p1 pToRemover, removePure p2 pToRemover)
  | _ -> if comparePure p pToRemover then TRUE else p 
  

let getInterval pure guard : (interval * pure) option = 
  match guard with 
  | Lt(Var i, t) -> 
    if entailConstrains pure (Eq (Var i, Num 0)) then 
    let p' = removePure pure (Eq (Var i, Num 0)) in 
    Some ((Num 0, Minus(t, Num 1)), PureAnd(p', Eq(Var i, t))) 
    else None  
  | _ -> None 


let invariantInference (index:term) (inv:interval) (body:effect) : (regularExpr * futureCond) = 
  let body = postProcess body in 

  debug_Inv_Infer("loopbodyEff after postProcess" ^  string_of_effect body);
  let (low, high) = inv in 
  let inv' = inv in 
  let (bagtrace:((pure * regularExpr)list)) = List.map ~f:(fun (_, p, es, _, _, _) -> (p, es)) body in 
  let (bagFC:((pure * regularExpr)list)) = (List.map ~f:(fun (_, p, _, fc , _, _) -> (p, fc)) body) in 
  Singleton (Bag(inv', bagtrace)), (Singleton (Bag(inv', bagFC))
)


let rec mutableTermCoreLang (stmts:core_lang) : term list = 
  match stmts with 
  | CAssign (v, e, _) -> v:: mutableTermCoreLang e
  | CIfELse (_, e1, e2, _) 
  | CSeq (e1, e2) -> mutableTermCoreLang e1 @ mutableTermCoreLang e2 
  | _ -> []

let rec removeNonArrayAssignment (stmts:core_lang) : (core_lang) =  
  match stmts with 
  | CAssign (Member (t, _), e, fp) -> stmts
    (* remove the index, just use the t *)
  | CAssign (_, e, fp) -> CSkip fp
  | CIfELse (b, e1, e2, fp) -> 
    CIfELse (b, removeNonArrayAssignment e1, removeNonArrayAssignment e2, fp)

  | CSeq (e1, e2) -> CSeq (removeNonArrayAssignment e1, removeNonArrayAssignment e2)
  | _ -> stmts

let rec getArrayHandlerMappingsPure (p:pure) : (term * term) list = 
  match p with 
  | Eq (Member (t, li), _) 
  | Eq (_, Member (t, li)) 
  | GtEq (Member (t, li), _) 
  | GtEq (_, Member (t, li))
  | LtEq (Member (t, li), _) 
  | LtEq (_, Member (t, li))
  | Lt (Member (t, li), _) 
  | Lt (_, Member (t, li))
  | Gt (Member (t, li), _) 
  | Gt (_, Member (t, li)) ->  [(t, Member (t, li))]
  | PureAnd (p1, p2) -> getArrayHandlerMappingsPure p1 @ getArrayHandlerMappingsPure p2 
  | _ -> []

  

let rec getArrayHandlerMappings (stmts:core_lang) : (term * term) list =  
  match stmts with 
  | CAssign (Member (t, li), e, fp) -> [(t, Member (t, li))]
    (* remove the index, just use the t *)
  | CAssign (_, e, fp) -> []
  | CIfELse (b, e1, e2, _) -> 
    getArrayHandlerMappingsPure b @ 
    getArrayHandlerMappings e1 @ 
    getArrayHandlerMappings e2 
  | CSeq (e1, e2) -> getArrayHandlerMappings e1 @ getArrayHandlerMappings e2 
  | _ -> []


  

(* this function returns the inferred decreasingArgument in the form of string and the loop bound invaraint *)
let decreasingArgumentInference (pState:pure) (guard:pure) (body:core_lang) : (term * interval) option  = 
  let rec helper (li:term list) (g:pure) : (term * interval) option = 
    match g with 
    | Lt(t1, t2) -> 
      if existAux strict_compare_Term li t1 == false then None 
      else 
        if entailConstrains pState (Eq (t1, Num 0)) then 
        Some (t1, (Num 0, t2 )) 
        else None  
    | LtEq(t1, t2) -> 
      if existAux strict_compare_Term li t1 == false then None 
      else 
        if entailConstrains pState (Eq (t1, Num 0)) then 
        Some (t1, (Num 0, Plus(t2, Num 1))) 
        else None  
    | Gt(t1, t2) -> helper li (Lt(t2, t1)) 
    | GtEq(t1, t2) -> helper li (LtEq(t2, t1)) 
    | PureAnd(p1, p2) -> 
      (match helper li p1  with 
      | None -> helper li p2
      | Some res -> Some res
      )
    | _ -> None 
  in 

  let mutableTerms = mutableTermCoreLang body in 
  
  match mutableTerms with 
  | [] -> None 
  | li -> helper li guard
  
  