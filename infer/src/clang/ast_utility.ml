let retKeyword = "Return"
let finalReport = (ref "")
let verifier_counter: int ref = ref 0;;


let debug_print str = 
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

       

(*Arithimetic pure formulae*)
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


type signature = (string * (term) list * term) 


type core_value = term

type literal = string * (core_value list)

type event = | Pos of literal | Neg of literal | ANY

type firstEle = event


type regularExpr = 
  | Bot 
  | Emp 
  | Singleton of event  
  | Disjunction of (regularExpr * regularExpr)
  | Concate of (regularExpr * regularExpr)
  | Kleene of regularExpr 



type futureCond = 
  | SingleFC of regularExpr 
  | ConjFC of futureCond list

let fc_default = SingleFC (Kleene (Singleton ANY))

type effect = ((pure * regularExpr * futureCond) list)

type summary = signature * effect

let (summaries: (summary list)ref) = ref []



let verifier_getAfreeVar () :string  =
  let prefix = "v"
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
  | Nil -> "[]"
  | TCons (a, b) -> Format.asprintf "%s::%s" (string_of_term a) (string_of_term b)
  | TTrue -> "true"
  | TFalse -> "false"
  | TNot a -> Format.asprintf "not(%s)" (string_of_term a)
  | TAnd (a, b) -> Format.asprintf "(%s&&%s)" (string_of_term a) (string_of_term b)
  | TOr (a, b) -> Format.asprintf "%s || %s" (string_of_term a) (string_of_term b)
  | Var str -> str
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
    TRUE -> "⊤"
  | FALSE -> "⊥"
  | Gt (t1, t2) -> (string_of_term t1) ^ ">" ^ (string_of_term t2)
  | Lt (t1, t2) -> (string_of_term t1) ^ "<" ^ (string_of_term t2)
  | GtEq (t1, t2) -> (string_of_term t1) ^ ">=" ^ (string_of_term t2) (*"≥"*)
  | LtEq (t1, t2) -> (string_of_term t1) ^ "<=" ^ (string_of_term t2) (*"≤"*)
  | Eq (t1, t2) -> (string_of_term t1) ^ "=" ^ (string_of_term t2)
  | PureOr (p1, p2) -> "("^string_of_pure p1 ^ "∨" ^ string_of_pure p2^")"
  | PureAnd (p1, p2) -> "("^string_of_pure p1 ^ "∧" ^ string_of_pure p2^")"
  | Neg (Eq (t1, t2)) -> "("^(string_of_term t1) ^ "!=" ^ (string_of_term t2)^")"
  | Neg (Gt (t1, t2)) -> "("^(string_of_term t1) ^ "<=" ^ (string_of_term t2)^")"
  | Neg p -> "!(" ^ string_of_pure p^")"

let string_of_loc n = "@" ^ string_of_int n

let string_of_signature (str, args, ret) = 
  str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) (args@[ret]) "," ^ ")"
  
let string_of_event (ev:event) : string = 
  match ev with 
  | Pos (str, args) -> str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) args "," ^ ")"
  | Neg (str, args) -> "!" ^ str ^ "(" ^ string_with_seperator (fun a -> string_of_term a) args "," ^ ")"
  | ANY -> "_"

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



let rec stricTcompareTerm (term1:term) (term2:term) : bool =
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
    stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | (TNot t1, TNot t2) -> stricTcompareTerm t1 t2
  | (TList tLi1, TList tLi2) -> stricTcompareTermList tLi1 tLi2
  | (Member (t1, tLi1), Member (t2, tLi2)) -> stricTcompareTermList (t1::tLi1) (t2::tLi2)
  | (UNIT, UNIT) | (ANY, ANY) | (RES, RES) | (Nil, Nil) | (TTrue, TTrue) | (TFalse, TFalse) -> true
  | _ -> false

and  stricTcompareTermList li1 li2 = 
  match li1, li2 with 
  | [],  [] -> true 
  | x::xs, y::ys -> stricTcompareTerm x y && stricTcompareTermList xs ys
  | _ , _ -> false 

let rec compareTermList tl1 tl2 : bool = 
  match tl1, tl2 with 
  | [], [] -> true 
  | (x:: xs, y:: ys) -> stricTcompareTerm x y && compareTermList xs ys 
  | _ -> false 

let rec comparePure (pi1:pure) (pi2:pure):bool = 
  match (pi1 , pi2) with 
    (TRUE, TRUE) -> true
  | (FALSE, FALSE) -> true 
  | (Gt (t1, t11), Gt (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Lt (t1, t11), Lt (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (GtEq (t1, t11), GtEq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (LtEq (t1, t11), LtEq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Eq (t1, t11), Eq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (PureOr (p1, p2), PureOr (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (PureAnd (p1, p2), PureAnd (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (Neg p1, Neg p2) -> comparePure p1 p2
  | _ -> false


let normalise_terms (t:term) : term = 
  match t with 

  | Minus (Minus(_end, b), Minus(_end1, Plus(b1, inc))) -> 
    if stricTcompareTerm _end _end1 && stricTcompareTerm b b1 then inc 
    else t 

  | Minus(Plus((Var x),( Num n1)), Plus(Minus((Var x1),( Var y)), ( Num n2))) -> 
    if String.compare x x1 == 0 then 
      if (n2-n1) == 0 then ( Var y)
      else if n2-n1 > 0 then Minus(( Var y), ( Num (n2-n1)))
      else Plus(( Var y), ( Num (n2-n1)))
    else t

  
  | Minus (t1, t2) -> 
    if stricTcompareTerm t1 t2 then (Num 0)
    else 

    (match t2 with
    | Minus (t21, t3) -> 
      if stricTcompareTerm t1 t21 then t3 
      else t 
    | _ -> t )
    
  | _ -> t 

let rec nullable (eff:regularExpr) : bool = 
  match eff with 
  | Bot              -> false 
  | Emp            -> true 
  | Singleton _ -> false
  | Concate (eff1, eff2) -> nullable eff1 && nullable eff2  
  | Disjunction (eff1, eff2) -> nullable eff1 || nullable eff2  
  | Kleene _       -> false 


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
    | (Kleene _, _) -> es1
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

let rec string_of_fc (fc:futureCond) : string = 
  match fc with 
  | SingleFC re -> string_of_regularExpr re
  | ConjFC fcs -> string_with_seperator string_of_fc fcs " /\\ "


let rec normalise_fc (fc:futureCond) : futureCond = 
  match fc with 
  | SingleFC re -> SingleFC (normalise_es re)
  | ConjFC fcs -> ConjFC (List.map ~f:normalise_fc fcs)

let rec normalise_effect (summary:effect)  : effect = 
  let normalise_effect_a_pair (p, re, fc) = (normalise_pure p, normalise_es re, normalise_fc fc) in 
  match summary with 
  | [] -> []
  | x :: xs -> (normalise_effect_a_pair x)  ::  (normalise_effect xs)

let rec string_of_effect summary = 

  let string_of_a_pair (p, re, fc) = string_of_pure p ^ " ; " ^ string_of_regularExpr re ^ " ; " ^ string_of_fc fc  in 

  match summary with 
  | [] -> ""
  | [x] -> string_of_a_pair x
  | x :: xs -> string_of_a_pair x  ^ " \\/ " ^ string_of_effect xs 


let rec flattenList lili = 
  match lili with 
  | [] -> []
  | x :: xs -> List.append x (flattenList xs) 

let cartesian_product li1 li2 = 
    flattenList (List.map li1 ~f:(fun l1 -> 
      List.map li2 ~f:(fun l2 -> (l1, l2))))

let concateSummaries s1 s2 = 
  let mixLi = cartesian_product s1 s2 in 
  let temp = (List.map mixLi ~f:(
    fun ((pi1, es_x),  (pi2, es_y)) -> 
      PureAnd(pi1, pi2), Concate (es_x, es_y) 
  )) in 
  temp

let rec reverse li = 
  match li with 
  | [] -> [] 
  | x :: xs  -> reverse(xs) @ [x]

let string_of_summary (signature, disjRE) = 
  string_of_signature signature ^ " = " ^ string_of_effect disjRE ^ "\n"

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
      if stricTcompareTerm formal t then Some arctual
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

let rec substitute_RE (re:regularExpr) (actual_formal_mappings:((term*term)list)): regularExpr = 
  match re with
  | Singleton ev  -> Singleton (substitute_event ev actual_formal_mappings) 
  | Concate (eff1, eff2) ->  
    Concate(substitute_RE eff1 actual_formal_mappings, substitute_RE eff2 actual_formal_mappings)
  | Disjunction (eff1, eff2) ->
    Disjunction(substitute_RE eff1 actual_formal_mappings, substitute_RE eff2 actual_formal_mappings)
     
  | Kleene effIn -> Kleene (substitute_RE effIn actual_formal_mappings)
  | _ -> re

let rec substitute_FC (fc:futureCond) (actual_formal_mappings:((term*term)list)): futureCond = 
    match fc with
    | SingleFC re -> SingleFC (substitute_RE re actual_formal_mappings)
    | ConjFC fcs -> ConjFC (List.map ~f:(fun fc -> substitute_FC fc actual_formal_mappings) fcs)
  

let substitute_effect (spec:effect) (actual_formal_mappings:((term*term)list)): effect =
  List.map ~f:(fun (p, re, fc) -> 
    let p' = substitute_pure p actual_formal_mappings in 
    let re' = substitute_RE re actual_formal_mappings in 
    let fc' = substitute_FC fc actual_formal_mappings in 

    (p', re', fc')) 
  spec



let rec getResTermFromRE re : term option =    
  match re with 
  | Singleton _  -> None
  | Concate (eff1, eff2) 
  | Disjunction (eff1, eff2) ->
      (match getResTermFromRE eff1, getResTermFromRE eff2 with 
      | None, None -> None 
      | _, Some t2 
      | Some t2, _ -> Some t2
      )
    
  | Kleene _  -> None 
  | _ -> None 


let rec getResTermFromeffect (re:effect) : (pure * term) list = 
  let (re:effect) = normalise_effect re in 
  let rec helper acc (p, re, fc) = 
    match getResTermFromRE re with 
    | None -> acc 
    | Some ret -> acc @ [(p, ret)] 
  in 
  List.fold_left ~init:[] ~f:helper re


(*
let rec derivative (ev:firstEle) (re:regularExpr) : regularExpr = 
  match re with 
  | Emp | Bot -> Bot 
  | Singleton(p, _) -> 
    (match ev with 
    | EPure (p1, _) -> if comparePure p p1 then Emp else Bot 
    | _ -> Bot 
    )
  | Concate(re1, re2) -> 
    let resRe1 =  Concate (derivative ev re1, re2) in 
    if nullable re1 then Disjunction(resRe1, derivative ev re2)
    else resRe1
  | Disjunction(re1, re2) -> Disjunction(derivative ev re1, derivative ev re2) 
  | Kleene reIn -> Concate (derivative ev reIn, re)

let rec removeIntermediateRes_regularExpr (re:regularExpr): regularExpr = 
  let re = normalise_es re in 
  match re with 
  | Kleene reIn -> Kleene (removeIntermediateRes_regularExpr reIn)
  | _ ->
  let fstLi = re_fst re in 
  let rec helper (evLi:firstEle list) : regularExpr = 
    match evLi with 
    | [] -> re 
    | [ev] ->  
      let deri = normalise_es (derivative ev re) in 
      (*
      print_endline ("re: " ^string_of_regularExpr re ); 
      print_endline ("derivatives: " ^string_of_regularExpr deri ); 
      *)
      (match deri with 
      | Emp -> 
        (match ev with 
        | EPure _ -> event2RegularExpression ev 
        | ECall((_, _, ret), state) -> 
          Concate (event2RegularExpression ev, 
          Singleton(Eq(RES, ret), state))

        )
      | _ -> 
        if containsIntermediateRes ev then 
        removeIntermediateRes_regularExpr deri
        else Concate (event2RegularExpression ev,removeIntermediateRes_regularExpr deri))
    | ev:: xs-> 
     let re1 = helper [ev] in 
     let re2 = helper xs in 
     Disjunction (re1, re2)
  in 
  helper fstLi 


let removeIntermediateRes_effect (disj_re:effect) : effect = 
  List.map ~f:(fun (p, re, fc) -> p, removeIntermediateRes_regularExpr re, fc) disj_re

*)


