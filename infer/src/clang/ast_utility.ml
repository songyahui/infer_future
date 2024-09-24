let retKeyword = "Return"
let nonDetermineFunCall = ["_fun__nondet_int";"_fun___VERIFIER_nondet_int"]


type basic_type = BINT of int | BVAR of string | BNULL | BRET | ANY | BSTR of string

type state = int

type terms = Basic of basic_type 
           | Plus of terms * terms
           | Minus of terms * terms 
       
(*Arithimetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of terms * terms
          | Lt of terms * terms
          | GtEq of terms * terms
          | LtEq of terms * terms
          | Eq of terms * terms
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure
          | Predicate of (string * terms list)

type regularExpr = 
  | Bot 
  | Emp 
  | Singleton of (pure * state)
  | Disjunction of (regularExpr * regularExpr)
  | Concate of (regularExpr * regularExpr)
  | Omega of regularExpr 

type summary = (pure * regularExpr) list 

type stack = (Exp.t * Ident.t) list


let rec existAux f (li:('a list)) (ele:'a) = 
  match li with 
  | [] ->  false 
  | x :: xs -> if f x ele then true else existAux f xs ele

let string_of_stack (stack:stack): string = 
  (String.concat ~sep:",\n" (List.map ~f:(fun (a, b) -> Exp.to_string a ^ " -> " ^ IR.Ident.to_string b) stack))

let string_of_basic_t v = 
  match v with 
  | BVAR name -> name
  | BINT n -> string_of_int n
  | BNULL -> "NULL"
  | BRET -> "ret"
  | ANY -> "_"
  | BSTR s -> "\"" ^ s ^ "\""



let rec string_of_terms (t:terms):string = 
  match t with
  | Basic v -> string_of_basic_t v 
  | Plus (t1, t2) -> "(" ^ (string_of_terms t1) ^ ("+") ^ (string_of_terms t2) ^ ")" 
  | Minus (t1, t2) -> "(" ^  (string_of_terms t1) ^ ("-") ^ (string_of_terms t2) ^ ")" 


let rec string_of_list_terms tL: string = 
  match tL with 
  | [] -> ""
  | [t] -> string_of_terms t 
  | x :: xs ->  string_of_terms x ^", "^ string_of_list_terms xs 


let rec string_of_pure (p:pure):string =   
  match p with
    TRUE -> "⊤"
  | FALSE -> "⊥"
  | Gt (t1, t2) -> (string_of_terms t1) ^ ">" ^ (string_of_terms t2)
  | Lt (t1, t2) -> (string_of_terms t1) ^ "<" ^ (string_of_terms t2)
  | GtEq (t1, t2) -> (string_of_terms t1) ^ ">=" ^ (string_of_terms t2) (*"≥"*)
  | LtEq (t1, t2) -> (string_of_terms t1) ^ "<=" ^ (string_of_terms t2) (*"≤"*)
  | Eq (t1, t2) -> (string_of_terms t1) ^ "=" ^ (string_of_terms t2)
  | PureOr (p1, p2) -> "("^string_of_pure p1 ^ "∨" ^ string_of_pure p2^")"
  | PureAnd (p1, p2) -> "("^string_of_pure p1 ^ "∧" ^ string_of_pure p2^")"
  | Neg (Eq (t1, t2)) -> "("^(string_of_terms t1) ^ "!=" ^ (string_of_terms t2)^")"
  | Neg (Gt (t1, t2)) -> "("^(string_of_terms t1) ^ "<=" ^ (string_of_terms t2)^")"
  | Neg p -> "!(" ^ string_of_pure p^")"
  | Predicate (str, termLi) -> str ^ "(" ^ string_of_list_terms termLi ^ ")"

let string_of_loc n = "@" ^ string_of_int n



let rec string_of_regularExpr re = 
  match re with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐 " 
  | Singleton (p, state)  -> "(" ^string_of_pure p  ^ ")"^ string_of_loc state
  | Concate (eff1, eff2) -> string_of_regularExpr eff1 ^ " · " ^ string_of_regularExpr eff2 
  | Disjunction (eff1, eff2) ->
      "((" ^ string_of_regularExpr eff1 ^ ") \\/ (" ^ string_of_regularExpr eff2 ^ "))"
     
  | Omega effIn          ->
      "(" ^ string_of_regularExpr effIn ^ ")^w"



let compareBasic_type (bt1:basic_type) (bt2:basic_type) : bool = 
  match (bt1, bt2) with 
  | (BSTR s1, BSTR s2)
  | ((BVAR s1), (BVAR s2)) -> String.compare s1 s2 == 0
  | (BINT n1, BINT n2) -> if n1 - n2 == 0 then true else false  
  | (BNULL, BNULL)
  | (ANY, ANY)
  | (BRET, BRET) -> true 
  | _ -> false 

let rec stricTcompareTerm (term1:terms) (term2:terms) : bool = 
  match (term1, term2) with 
  | (Basic t1, Basic t2) -> compareBasic_type t1 t2
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | _ -> false 

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
  | (Predicate (s1, tLi1), Predicate (s2, tLi2)) -> 
    String.compare s1 s2 == 0  &&  compareTermList tLi1 tLi2
  | _ -> false


let normalise_terms (t:terms) : terms = 
  match t with 

  | Minus (Minus(_end, b), Minus(_end1, Plus(b1, inc))) -> 
    if stricTcompareTerm _end _end1 && stricTcompareTerm b b1 then inc 
    else t 

  | Minus(Plus(Basic(BVAR x),Basic( BINT n1)), Plus(Minus(Basic(BVAR x1),Basic( BVAR y)), Basic( BINT n2))) -> 
    if String.compare x x1 == 0 then 
      if (n2-n1) == 0 then Basic( BVAR y)
      else if n2-n1 > 0 then Minus(Basic( BVAR y), Basic( BINT (n2-n1)))
      else Plus(Basic( BVAR y), Basic( BINT (n2-n1)))
    else t

  
  | Minus (t1, t2) -> 
    if stricTcompareTerm t1 t2 then Basic(BINT 0)
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
  | Omega _       -> false 


let rec normalise_pure (pi:pure) : pure = 
  match pi with 
  | TRUE 
  | FALSE -> pi


  | LtEq (Basic(BINT n), Basic(BVAR v)) -> GtEq (Basic(BVAR v), Basic(BINT n))
  | Lt (Basic(BINT n), Basic(BVAR v)) -> Gt (Basic(BVAR v), Basic(BINT n))
  | Gt (Basic(BINT n), Basic(BVAR v)) -> Lt (Basic(BVAR v), Basic(BINT n))

  | Gt (leftHandside,Basic( BINT 0)) -> 
    (match normalise_terms leftHandside with
    | Minus(t1, t2) -> Gt (t1, t2)
    | Plus(t1, Basic( BINT n)) -> Gt (t1,  Basic( BINT (-1 * n)))
    | t -> Gt(t, Basic( BINT 0))
    )

  
    
  | LtEq (Minus(t1, t2),Basic( BINT 0)) -> LtEq (t1, t2)

  | Gt (Minus(Basic(BINT n1),Basic( BVAR v1)),Basic( BINT n2)) -> Lt(Basic(BVAR v1), Basic (BINT(n1-n2)))
  


  | Gt (t1, t2) -> Gt (normalise_terms t1, normalise_terms t2)

  | Lt (t1, t2) -> Lt (normalise_terms t1, normalise_terms t2)
  | GtEq (t1, t2) -> GtEq (normalise_terms t1, normalise_terms t2)

  | LtEq (Minus(Basic(BVAR x),Basic( BINT n1)), Minus(Minus(Basic(BVAR x1),Basic( BVAR y)), Basic( BINT n2))) -> 
    if String.compare x x1 == 0 then  LtEq(Basic(BVAR y), Basic( BINT (n2-n1)))
    else LtEq (normalise_terms (Minus(Basic(BVAR x),Basic( BINT n1))), normalise_terms (Minus(Minus(Basic(BVAR x1),Basic( BVAR y)), Basic( BINT n2))))

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
  | Predicate (str, termLi) -> 
    Predicate (str, List.map termLi ~f:(normalise_terms))

   
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
    | (Singleton (TRUE, _), _)
    | (Emp, _) -> normalise_es es2
    | (_, Singleton (TRUE, _))
    | (_, Emp) -> normalise_es es1
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Omega _, _) -> es1
    (*| (Disjunction (es11, es12), es3) -> Disjunction(normalise_es (Concate (es11,es3)),  normalise_es (Concate (es12, es3))) *)
    | (Concate (es11, es12), es3) -> (Concate (es11, normalise_es (Concate (es12, es3))))
    | _ -> (Concate (es1, es2))
    )
  | Omega effIn -> 
    let effIn' = normalise_es effIn in 
    Omega (effIn')



  | Singleton (p, state) ->  Singleton (normalise_pure p, state)

  | _ -> eff 


let rec normalise_summary summary = 

  let normalise_summary_a_pair (p, re) = (normalise_pure p, normalise_es re) in 

  match summary with 
  | [] -> []
  | x :: xs -> (normalise_summary_a_pair x)  ::  (normalise_summary xs)

let rec string_of_summary summary = 

  let string_of_a_pair (p, re) = string_of_pure p ^ " /\\ " ^ string_of_regularExpr re  in 

  match summary with 
  | [] -> ""
  | [x] -> string_of_a_pair x
  | x :: xs -> string_of_a_pair x  ^ " \\/ " ^ string_of_summary xs 

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