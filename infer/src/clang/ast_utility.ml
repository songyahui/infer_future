open Z3


let errorCode_normal = 0 
let errorCode_return = -1 
let errorCode_exit = -2 

let errorCode_failure = -3


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
    if false  then debug_print (str)
    else ()
  

let debug_checkPostConditionError str = 
    if true  then debug_print (str)
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


let nonDetermineFunCall = ["__nondet_int";"__VERIFIER_nondet_int"]

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


type signature = (string * (term list)) 


type core_value = term

type literal = string * (core_value list)

type event = | Pos of literal | Neg of literal | NegTerm of term list | ANY

type firstEle = event


type regularExpr = 
  | Bot 
  | Emp 
  | Singleton of event  
  | Disjunction of (regularExpr * regularExpr)
  | Concate of (regularExpr * regularExpr)
  | Kleene of regularExpr 



type futureCond = regularExpr list 

let fc_default =  [(Kleene (Singleton ANY))]

type exitCode = int 

(* string list are the existential vars *)
type singleEffect = (string list * pure * regularExpr * futureCond * term * exitCode)
type effect = (singleEffect list)

type precondition = pure 

type summary = signature * precondition * effect

let (summaries: (summary list)ref) = ref []

let defultSingelEff = ([], TRUE, Emp, fc_default, Var "_" , 0 )

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

let rec existAux f (li:('a list)) (ele:'a) = 
  match li with 
  | [] ->  false 
  | x :: xs -> if f x ele then true else existAux f xs ele


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
  (* | Pointer str -> "*" ^ str
*)
  | Rel (bop, t1, t2) ->
    "(" ^ string_of_term t1 ^ (match bop with | EQ -> "==" | _ -> string_of_bin_op bop) ^ string_of_term t2 ^ ")"
  | Plus (t1, t2) -> "(" ^string_of_term t1 ^ "+" ^ string_of_term t2^ ")"
  | Minus (t1, t2) -> "(" ^string_of_term t1 ^ "-" ^ string_of_term t2 ^ ")"
  | TPower (t1, t2) -> "(" ^string_of_term t1 ^ "^(" ^ string_of_term t2 ^ "))"
  | TTimes (t1, t2) -> "(" ^string_of_term t1 ^ "*" ^ string_of_term t2 ^ ")"
  | TDiv (t1, t2) -> "(" ^string_of_term t1 ^ "/" ^ string_of_term t2 ^ ")"
  | Member (t1, t2) -> string_of_term t1 ^ "." ^ string_of_li string_of_term t2 "." 
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

let string_of_loc n = "@" ^ string_of_int n

let string_of_signature (str, args) = 
  str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) (args) "," ^ ")"
  
let string_of_event (ev:event) : string = 
  match ev with 
  | Pos (str, args) -> str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) args "," ^ ")"
  | Neg (str, args) -> "!" ^ str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) args "," ^ ")"
  | ANY -> "_"
  | NegTerm args -> "!_" ^ "(" ^ string_with_seperator (fun a -> string_of_term a) args "," ^ ")"

let rec string_of_regularExpr re = 
  match re with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐 " 
  | Singleton (ev)  ->  string_of_event ev 
  | Concate (eff1, eff2) -> string_of_regularExpr eff1 ^ " · " ^ string_of_regularExpr eff2 
  | Disjunction (eff1, eff2) ->
      "((" ^ string_of_regularExpr eff1 ^ ") \\/ (" ^ string_of_regularExpr eff2 ^ "))"
     
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


let normalise_terms (t:term) : term = 
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

(*8*******************************)


let rec nullable (eff:regularExpr) : bool = 
  match eff with 
  | Bot            -> false 
  | Emp            -> true 
  | Singleton _    -> false
  | Concate (eff1, eff2) -> nullable eff1 && nullable eff2  
  | Disjunction (eff1, eff2) -> nullable eff1 || nullable eff2  
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
  | Kleene re1 -> re_fst re1 

let compareTermListWithPure (p:pure) (t1:term list) (t2:term list) : bool = 
  let pairs = actual_formal_mappings t1 t2 in  
  let (equlity:pure) = List.fold_left ~f:(fun acc (t1, t2) ->  PureAnd (acc, Eq (t1, t2))) ~init:TRUE pairs  in
  if entailConstrains p equlity then true 
  else false
  ;;


let derivativeEvent (p:pure) (ev:firstEle) (evTarget:firstEle) : regularExpr = 
  match evTarget with 
  | ANY -> Emp
  | Pos (strTarget, argsTarget) -> 
    (match ev with
    | Pos (str, args) -> 
      if String.compare str strTarget == 0 && List.length args == List.length argsTarget then 
        if compareTermListWithPure p args argsTarget  then Emp 
        else Bot
      else Bot
    | _ -> Bot
    )
  | Neg (strTarget, argsTarget) -> 
    (match ev with
    | Pos (str, args) -> 
      if String.compare str strTarget != 0 || List.length args != List.length argsTarget then Emp 
      else if compareTermListWithPure p args argsTarget == false then Emp 
      else Bot  
    | _ -> Bot
    )
  | NegTerm argsTarget -> 
    (match ev with
    | Pos (str, args) -> 
      if compareTermListWithPure p args argsTarget then Bot  
      else Emp
    | _ -> Bot
    )
  


let rec derivative (p:pure) (ev:firstEle) (re:regularExpr) : regularExpr = 
  match re with 
  | Emp | Bot -> Bot 
  | Singleton evIn -> derivativeEvent p ev evIn 
   
  | Concate(re1, re2) -> 
    let resRe1 = Concate (derivative p ev re1, re2) in 
    if nullable re1 then Disjunction(resRe1, derivative p ev re2)
    else resRe1
  | Disjunction(re1, re2) -> Disjunction(derivative p ev re1, derivative p ev re2) 
  | Kleene reIn -> Concate (derivative p ev reIn, re)


let rec normalise_pure (pi:pure) : pure = 
  match pi with 
  | TRUE 
  | FALSE -> pi
  | LtEq ((Num n), (Var v)) -> GtEq ((Var v), (Num n))
  | Lt ((Num n), (Var v)) -> Gt ((Var v), (Num n))
  | Gt ((Num n), (Var v)) -> Lt ((Var v), (Num n))

  | Gt (leftHandside,( Num 0)) -> 
    (match normalise_terms leftHandside with
    | Minus(t1, t2) -> Gt (t1, t2)
    | Plus(t1, ( Num n)) -> Gt (t1,  ( Num (-1 * n)))
    | t -> Gt(t, ( Num 0))
    )
  | LtEq (Minus(t1, t2),( Num 0)) -> LtEq (t1, t2)
  | Gt (Minus((Num n1),( Var v1)),( Num n2)) -> Lt((Var v1),  (Num(n1-n2)))
  | Gt (t1, t2) -> Gt (normalise_terms t1, normalise_terms t2)
  | Lt (t1, t2) -> Lt (normalise_terms t1, normalise_terms t2)
  | GtEq (t1, t2) -> GtEq (normalise_terms t1, normalise_terms t2)
  | LtEq (Minus((Var x),( Num n1)), Minus(Minus((Var x1),( Var y)), ( Num n2))) -> 
    if String.compare x x1 == 0 then  LtEq((Var y), ( Num (n2-n1)))
    else LtEq (normalise_terms (Minus((Var x),( Num n1))), normalise_terms (Minus(Minus((Var x1),( Var y)), ( Num n2))))

  | LtEq (t1, t2) -> LtEq (normalise_terms t1, normalise_terms t2)
  | Eq (t1, t2) -> Eq (normalise_terms t1, normalise_terms t2)
  | PureAnd (pi1,pi2) -> 
    let p1 = normalise_pure pi1 in 
    let p2 = normalise_pure pi2 in 
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
  | Neg piN -> Neg (normalise_pure piN)
  | PureOr (pi1,pi2) -> PureAnd (normalise_pure pi1, normalise_pure pi2)
  | Exists (str, p) -> Exists (str, normalise_pure p)
   
let rec normalise_es (eff:regularExpr) : regularExpr = 
  match eff with 
  | Disjunction(es1, es2) -> 
    let es1 = normalise_es es1 in 
    let es2 = normalise_es es2 in 
    (match (es1, es2) with 
    | (Emp, Emp) -> Emp
    | (Emp, _) -> if nullable es2 then es2 else (Disjunction (es2, es1))
    | (Bot, es) -> normalise_es es 
    | (es, Bot) -> normalise_es es 
    | _ -> (Disjunction (es1, es2))
    )
  | Concate (es1, es2) -> 
    let es1 = normalise_es es1 in 
    let es2 = normalise_es es2 in 
    (match (es1, es2) with 
    | (Emp, _) -> normalise_es es2
    | (_, Emp) -> normalise_es es1
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    (*| (Disjunction (es11, es12), es3) -> Disjunction(normalise_es (Concate (es11,es3)),  normalise_es (Concate (es12, es3))) *)
    | (Concate (es11, es12), es3) -> (Concate (es11, normalise_es (Concate (es12, es3))))
    | _ -> (Concate (es1, es2))
    )
  | Kleene effIn -> 
    let effIn' = normalise_es effIn in 
    Kleene (effIn')

  | _ -> eff 

let string_of_loc n = "@" ^ string_of_int n 

let rec string_of_core_lang (e:core_lang) :string =
  match e with
  | CValue (v, state) -> string_of_term v ^ string_of_loc state 
  | CAssign (v, e, state) -> Format.sprintf "%s=%s " (string_of_term v) (string_of_core_lang e) ^ string_of_loc state 
  | CIfELse (pi, t, e, state) -> Format.sprintf "if (%s) then %s else (%s)" (string_of_pure pi)  (string_of_core_lang t) (string_of_core_lang e) ^ string_of_loc state
  | CFunCall (f, xs, state) -> Format.sprintf "%s(%s)" f (List.map ~f:string_of_term xs |> String.concat ~sep:",") ^ string_of_loc state 
  | CLocal (str, state) -> Format.sprintf "local %s " str ^ string_of_loc state 
  | CSeq (e1, e2) -> Format.sprintf "%s\n%s" (string_of_core_lang e1) (string_of_core_lang e2) 
  | CWhile (pi, e, state) -> Format.sprintf "while (%s)\n {%s}" (string_of_pure pi) (string_of_core_lang e) ^ string_of_loc state 
  | CBreak  state ->  "Break" ^ string_of_loc state
  | CContinue state -> "Continue" ^ string_of_loc state
  | CLable (str, state) ->  str ^ ": " ^ string_of_loc state
  | CGoto (str, state) -> "goto " ^ str ^ " " ^ string_of_loc state

let rec string_of_fc (fc:futureCond) : string = string_with_seperator (fun a -> "("^string_of_regularExpr a ^")") fc " /\\ "

let rec removeAny fcIn = 
  if List.length fcIn <= 1 then fcIn 
  else
    let temp = 
    List.filter ~f:(fun a -> match a with | Kleene (Singleton ANY) -> false | _ -> true) fcIn in 
    if List.length temp == 0 then [Kleene (Singleton ANY)]
    else temp

let normalise_fc (fc:futureCond) : futureCond = 
  let rec existBot (fcIn:futureCond) : bool =
    match fcIn with 
    | [] -> false 
    | Bot :: _ -> true 
    | _ :: xs -> existBot xs 
  in 
  (*debug_print ("original_fc: " ^ string_of_fc fc ); *) 
  let fc' = (List.map ~f:normalise_es fc) in 
  (*debug_print ("normalised fc: " ^ string_of_fc fc' ); *)
  if existBot fc' then [Bot] 
  else removeAny fc' 




let rec normalise_effect (summary:effect)  : effect = 
  let normalise_effect_a_pair (exs, p, re, fc, r, exitCode) = 
    let p' = normalise_pure p in
    if entailConstrains p' FALSE then []
    else [(exs, normalise_pure p, normalise_es re, normalise_fc fc, r, exitCode)] in 
  match summary with 
  | [] -> []
  | x :: xs -> (normalise_effect_a_pair x)  @  (normalise_effect xs)

let string_of_exs exs = string_with_seperator (fun a -> a) exs " "

let string_of_single_effect (exs, p, re, fc, r, exitCode) = 
  (if List.length exs == 0 then ":" else "∃" ^  string_of_exs exs ^  ": ") ^ 
  string_of_pure p ^ " ; " ^ 
  string_of_regularExpr re ^ " ; " ^ 
  string_of_fc fc   ^ " ; " ^ 
  string_of_term r  ^  
  (if exitCode < 0 then " ERROR PATH! "  else "" )
  (* ^ string_of_int exitCode  *)


let rec string_of_effect (summary:effect) = 

  let string_of_a_pair (exs, p, re, fc, r, exitCode) = string_of_single_effect (exs, p, re, fc, r, exitCode) in 

  match summary with 
  | [] -> ""
  | [x] -> "(" ^ string_of_a_pair x ^ ")"
  | x :: xs -> "(" ^ string_of_a_pair x  ^ ") \\/ \n    " ^ string_of_effect xs 


let rec flattenList lili = 
  match lili with 
  | [] -> []
  | x :: xs -> List.append x (flattenList xs) 

let cartesian_product li1 li2 = 
    flattenList (List.map li1 ~f:(fun l1 -> 
      List.map li2 ~f:(fun l2 -> (l1, l2))))

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
    

let substitute_term_aux (t:term) (actual_formal_mappings:((term*term)list)): term = 
  let rec helper li : term option  = 
    match li with 
    | [] -> None 
    | (arctual, formal)::xs  -> 
      if strict_compare_Term formal t then Some arctual
      else helper xs 
  in 
  match helper actual_formal_mappings with 
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
    let b' =List.map b ~f:(fun a -> substitute_term a actual_formal_mappings) in 
    Member (substitute_term a actual_formal_mappings, b')
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

let substitute_event (ev:event) (actual_formal_mappings:((term*term)list)): event = 
  match ev with 
  | Pos (str, args) -> Pos (str, List.map ~f:(fun a -> substitute_term a actual_formal_mappings) args)
  | Neg (str, args) -> Neg (str, List.map ~f:(fun a -> substitute_term a actual_formal_mappings) args)
  | ANY -> ANY
  | NegTerm (args) -> NegTerm (List.map ~f:(fun a -> substitute_term a actual_formal_mappings) args)

let rec substitute_RE (re:regularExpr) (actual_formal_mappings:((term*term)list)): regularExpr = 
  match re with
  | Singleton ev  -> Singleton (substitute_event ev actual_formal_mappings) 
  | Concate (eff1, eff2) ->  
    Concate(substitute_RE eff1 actual_formal_mappings, substitute_RE eff2 actual_formal_mappings)
  | Disjunction (eff1, eff2) ->
    Disjunction(substitute_RE eff1 actual_formal_mappings, substitute_RE eff2 actual_formal_mappings)
     
  | Kleene effIn -> Kleene (substitute_RE effIn actual_formal_mappings)
  | _ -> re

let rec substitute_FC (fc:futureCond) (actual_formal_mappings:((term*term)list)): futureCond = (List.map ~f:(fun fce -> substitute_RE fce actual_formal_mappings) fc)

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


let getAllTermsFromEvent (ev:event) : term list = 
  match ev with 
  | Pos (_, args)
  | Neg (_, args)
  | NegTerm args -> args 
  | ANY -> []

let rec getAllTermsFromRE re : term list =    
  match re with 
  | Singleton ev  -> getAllTermsFromEvent ev
  | Concate (eff1, eff2) 
  | Disjunction (eff1, eff2) -> getAllTermsFromRE eff1 @ getAllTermsFromRE eff2
  | Kleene reIn  -> getAllTermsFromRE reIn  
  | Emp | Bot  -> [] 

let getAllTermsFromFC (fc:futureCond) : term list =
  List.fold_left ~f:(fun acc re -> acc @ getAllTermsFromRE re) ~init:[] fc

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
    

let rec findATermEquleToX (p:pure) (x:string) : term list = 
  match p with
  | Eq (Var a, Var b) -> 
    if String.compare a b == 0 then [] 
    else 
      if String.compare a x == 0 then [Var b]
      else if String.compare b x == 0 then [Var a]
      else []
  | Eq (Var a, b) 
  | Eq (b, Var a) -> 
    (match b with 
    | ANY -> [] 
    | _ -> 
      if strict_compare_Term (Var a) b then [] 
      else if String.compare a x == 0 then [b]
      else []
    )
    
  | PureAnd (p1, p2) -> findATermEquleToX p1 x @ findATermEquleToX p2 x
  | _ -> []

let rec findReplacebleVar exs p : (string * term) option = 
  match exs with 
  | [] -> None 
  | x :: xs -> 
    let allTermEquleToX = findATermEquleToX p x in 
    (match allTermEquleToX with 
    | [] -> findReplacebleVar xs p 
    | y::_ -> Some (x, y)
    )

;;

let rec removeIntermediateRes exs tobereplacedList = 
  match exs with
  | [] -> [] 
  | x :: xs -> 
    if existAux (fun a b -> if String.compare a b == 0 then true else false) tobereplacedList x then removeIntermediateRes xs tobereplacedList
    else x:: removeIntermediateRes xs tobereplacedList





let rec removeIntermediateResHelper  (exs, p, re, fc, r, exitCode) : singleEffect = 
  match findReplacebleVar exs p with 
  | None -> (exs, p, re, fc, r, exitCode)
  | Some (tobereplaced, (ex:term)) -> 
    debug_postprocess ("\n========\nex: " ^ string_of_term ex);
    debug_postprocess ("tobereplacedList: " ^ tobereplaced );
    let mappings =  [(ex, Var tobereplaced)] in  
    let exs' = removeIntermediateRes exs [tobereplaced] in 
    let temp = (exs', substitute_pure p mappings, substitute_RE re mappings, substitute_FC fc mappings, substitute_term r mappings, exitCode)  in 

    debug_postprocess ("temp: " ^ string_of_effect [temp]);

    removeIntermediateResHelper  temp

  ;;


let removeIntermediateRes  ((exs, p, re, fc, r, exitCode):singleEffect) : singleEffect =
  let (a, b, c, d, r, ec) = removeIntermediateResHelper  (exs, p, re, fc, r, exitCode) in
  (a, b, c, d, r, ec) 
;; 

let removeUnusedExs ((a, b, c, d, r, ec):singleEffect) : singleEffect =

  let allTerms = getAllTermsFromPure b @ getAllTermsFromRE c @ getAllTermsFromFC d @ [r] in 
  let a' = List.filter ~f:(fun ex -> existAux strict_compare_Term allTerms (Var ex)) a in 
  (a', b, c, d, r, ec)

  
let postProcess  (eff:effect) : effect = 
  let rec helper (eff:effect) : effect = 
    match eff with 
    | [] -> []
    | single :: xs -> 
      removeUnusedExs (removeIntermediateRes  single) :: helper xs
  in 
  helper eff

let remove_duplicates f lst =
  let rec aux seen = function
    | [] -> List.rev seen
    | hd::tl ->
      if List.exists ~f:(fun x -> f x hd) seen then aux seen tl
      else aux (hd::seen) tl
  in
  aux [] lst

let rec existTermInForall (fc_Vars:term list) (forallVar:term list) : bool = 
  match fc_Vars with 
  | [] -> false 
  | x :: xs  -> if existAux strict_compare_Term forallVar x then true else existTermInForall xs forallVar

let rec notExistTermInForall (fc_Vars:term list) (forallVar:term list) : bool = 
  match fc_Vars with 
  | [] -> false 
  | x :: xs  -> if existAux strict_compare_Term forallVar x  == false then true else notExistTermInForall xs forallVar


let decompositeFCByForallExists (fc:futureCond) (forallVar:term list): (futureCond * futureCond) = 
  List.fold_left ~f:(fun (fcInputOutput, fcExists) fcCurrent -> 
    let fc_Vars = remove_duplicates strict_compare_Term (getAllTermsFromFC [fcCurrent]) in 
    match fc_Vars with 
    | [] -> (fcInputOutput@[fcCurrent], fcExists)
    | fc_Vars ->  
      let fcInputOutput' = if existTermInForall fc_Vars forallVar then fcInputOutput@[fcCurrent] else fcInputOutput in 
      let fcExists' = if notExistTermInForall fc_Vars forallVar then fcExists@[fcCurrent] else fcExists in 
      (fcInputOutput', fcExists')
  ) ~init:(fc_default, fc_default) fc
  ;;

let checkPostConditionError (eff:effect) (formalArgs:term list) : effect = 
  let aux ((exs, p, re, fc, r, exitCode):singleEffect) : singleEffect option = 
    match fc with 
    | [Bot] -> (* if already error, then no need to check *) Some (exs, p, re, fc, r, exitCode)
    | _ -> 
      let inputOutputTerms = r::formalArgs in 
      let fcInputOutput, fcExists = decompositeFCByForallExists fc inputOutputTerms in 
      debug_checkPostConditionError("fcForall = " ^ string_of_fc fcInputOutput);
      debug_checkPostConditionError("fcExists = " ^ string_of_fc fcExists); 
      (*
      if traceInclusion Emp fcExists ... 
      *)
      Some (exs, p, re, fc, r, exitCode)
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
  | [] -> [defultSingelEff] 
  | eff -> eff