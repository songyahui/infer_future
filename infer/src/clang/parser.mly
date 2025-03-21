%{ open Ast_utility %}
%{ open List %}

(*%token <string> EVENT*)
%token <string> VAR
%token <int> INTE
%token EMPTY LPAR RPAR CONCAT  POWER  DISJ  PureConj UNIT Exists
%token COLON  REQUIRE ENSURE FUTURESpec LSPEC RSPEC NULL SEMI
%token UNDERLINE KLEENE EOF BOTTOM NOTSINGLE RETURN
%token GT LT EQ GTEQ LTEQ CONJ COMMA MINUS 
%token PLUS TRUE FALSE 
%token FUTURE GLOBAL IMPLY LTLNOT NEXT UNTIL LILAND LILOR
%left DISJ 
%left CONCAT



%start summary
%type <(Ast_utility.summary)> summary
%type <(Ast_utility.signature)> signature
%type <(Ast_utility.effect)> effect
%type <(Ast_utility.singleEffect)> singleEffect
%type <(Ast_utility.pure)> pure
%type <(Ast_utility.term)> term
%type <(Ast_utility.regularExpr)> es
%type <(Ast_utility.event)> event
%type <(Ast_utility.literal)> literal
%type <(Ast_utility.futureCond)> futureCond
(*

%type <(Ast_utility.effect option * Ast_utility.effect option * Ast_utility.effect option)> optionalPrecondition
%type <(Ast_utility.effect option * Ast_utility.effect option)> optionalPostcondition
%type <(Ast_utility.effect option)> optionalFuturecondition
%type <(Ast_utility.basic_type list)> parm
%type <(Ast_utility.es)> es
%type <(string list)> formalparm
%type <(Ast_utility.effect)> effect
%type <(Ast_utility.es)> es_or_ltl
%type <(Ast_utility.ltl)> ltl


*)

%type <(Ast_utility.term list)> list_of_formalArgs

%type <(Ast_utility.term list)> list_of_terms
%type <(string list)> list_of_exs
%type <(Ast_utility.literal)>  not_event


%%

list_of_formalArgs:
| {[]}
| s = VAR {[(Var s)]}
| li=list_of_formalArgs COMMA s=VAR {li@[(Var s)]} 





signature: 
| str = VAR  LPAR tLi=list_of_formalArgs RPAR  {(str, tLi)}

list_of_exs:
| {[]}
| s = VAR {[(s)]}
| li=list_of_exs s=VAR {li@[(s)]} 


term:
| UNIT {UNIT}
| UNDERLINE {ANY}
| i = INTE{Num ( i) }
| v = VAR {Var v} 
| LPAR r = term RPAR { r }
| a = term b = INTE {Minus (a, ( Num  (0 -  b)))}
| LPAR a = term MINUS b = term RPAR {Minus (a, b )}
| LPAR a = term PLUS b = term RPAR {Plus (a, b)}

list_of_terms:
| {[]}
| t = term {[t]}
| li=list_of_terms COMMA t = term {li@[t]} 


literal:
| str=VAR LPAR tLi=list_of_terms  RPAR {(str, tLi)}

not_event:
| l=literal { l}
| UNDERLINE LPAR tLi=list_of_terms  RPAR {("_", tLi)}

event:
| UNDERLINE {ANY}
| NOTSINGLE l= not_event{Neg l }
| l=literal {Pos l}


es:
| BOTTOM {Bot}
| EMPTY {Emp}
| ev=event { Singleton (ev) }
| LPAR r = es RPAR { r }
| a = es DISJ b = es { Disjunction(a, b) }
| a = es CONCAT b = es { Concate (a, b) } 
| LPAR a = es  RPAR POWER KLEENE {Kleene a}


pure:
| TRUE {TRUE}
| FALSE {FALSE}
| NOTSINGLE LPAR a = pure RPAR {Neg a}
| LPAR r = pure RPAR { r }
| a = term GT b = term {Gt (a, b)}
| a = term LT b = term {Lt (a, b)}
| a = term GTEQ b = term {GtEq (a, b)}
| a = term LTEQ b = term {LtEq (a, b)}
| a = term EQ b = term {Eq (a, b)}
| a = pure PureConj b = pure {PureAnd (a, b)}
| a = pure DISJ b = pure {PureOr (a, b)}
| Exists strs= list_of_exs CONCAT a = pure {Exists ( strs, a)}

futureCond: 
| s = es {[s]}
| li= futureCond CONJ s = es {li@[s]}

singleEffect: LPAR strs= list_of_exs COLON p=pure SEMI es=es SEMI fc=futureCond SEMI r=VAR RPAR {(strs, p, es, fc, Var r)}

effect:
| s = singleEffect {[s]}
| li= effect DISJ s = singleEffect {li@[s]}

summary:
| LSPEC s=signature EQ REQUIRE p=pure  ENSURE  eff=effect   
  RSPEC {(s, p,  eff)}




(*
parm:
| LPAR RPAR {[]}
| LPAR argument= basic_type RPAR {[argument]}


anyEventOrAny:
| {Any}
| p=parm { Singleton (("_", p), None) }

neGationAny:
| UNDERLINE p=parm  { NotSingleton (("_", p))}
| str = VAR p=parm { NotSingleton ((str, p))}



ltl : 
| str = VAR p=parm {Lable (str, p)} 
| LPAR r = ltl RPAR { r }
| NEXT p = ltl  {Next p}
| LPAR p1= ltl UNTIL p2= ltl RPAR {Until (p1, p2)}
| GLOBAL p = ltl {Global p}
| FUTURE p = ltl {Future p}
| LTLNOT p = ltl {NotLTL p}
| LPAR p1= ltl IMPLY p2= ltl RPAR {Imply (p1, p2)}
| LPAR p1= ltl LILAND p2= ltl RPAR {AndLTL (p1, p2)}  
| LPAR p1= ltl LILOR p2= ltl RPAR {OrLTL (p1, p2)}

es_or_ltl:
| COMMA  b= es  {b}
| COLON b = ltl {
  let rec ltlToEs l = 
    match l with 
    | Lable str ->  Singleton (str, None)
    | Next ltl -> Concatenate (Any, ltlToEs ltl)
    | Global ltl -> Kleene (ltlToEs ltl)
    | Future ltl -> Concatenate (Kleene Any, ltlToEs ltl)
    | OrLTL (ltl1, ltl2) -> Disj (ltlToEs ltl1, ltlToEs ltl2)
    | NotLTL (Lable str) ->  NotSingleton str 
    | Imply (Lable str, ltl2) -> Disj (NotSingleton str ,  ltlToEs ltl2)
    | Until (ltl1, ltl2) -> Concatenate(Kleene (ltlToEs ltl1), ltlToEs ltl2)
    (*
    | NotLTL of ltl 
    | Imply of ltl * ltl
    | AndLTL of ltl * ltl
     *)
    | _ ->  Singleton (("ltlToEs not yet", []), None)
  in ltlToEs b 
}

effect:
| LPAR r = effect RPAR { r }
| a = pure  b = es_or_ltl {[(a, b)]}
| a = effect  DISJ  b=effect  {List.append a b}



optionalFuturecondition:
| {None}
| FUTURESpec e3 = effect {Some e3}

optionalPostcondition:
| ENSURE e2 = effect a = optionalFuturecondition {
  (Some e2, a)
}
| a = optionalFuturecondition {
  (None, a)
}

optionalPrecondition:
| REQUIRE e1 = effect a = optionalPostcondition 
{let (e2, e3) = a in 
  (Some e1, e2, e3)}
| a = optionalPostcondition 
{let (e2, e3) = a in 
  (None, e2, e3)}


formalparmRest:
| {None}
| COMMA rest=formalparm {Some rest}

formalparm:
| {[]}
| str = variable rest = formalparmRest {
  match rest with 
  | None  -> [str]
  | Some rest -> str ::rest
  }


specification: 
| EOF {(("", []), None, None, None)}
| LSPEC str = variable LPAR argument=formalparm RPAR COLON 
a = optionalPrecondition 
RSPEC {
  let (e1, e2, e3) = a in 
  ((str, argument), e1, e2, e3)}


specification: 
| EOF {("", None, None, None)}
| LSPEC str = VAR COLON 
REQUIRE e1=effect ENSURE e2=effect FUTURESpec e3=effect
RSPEC {  (str, Some e1, Some e2, Some e3)}
*)
