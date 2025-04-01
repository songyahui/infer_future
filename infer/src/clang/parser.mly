%{ open Ast_utility %}
%{ open List %}

(*%token <string> EVENT*)
%token <string> VAR
%token <int> INTE
%token EMPTY LPAR RPAR CONCAT  POWER  DISJ  PureConj UNIT EXIST
%token COLON  REQUIRE ENSURE FUTURESpec LSPEC RSPEC NULL SEMI
%token UNDERLINE KLEENE EOF BOTTOM NOTSINGLE RETURN
%token GT LT EQ GTEQ LTEQ CONJ COMMA MINUS 
%token PLUS TRUE FALSE 
%token FUTURE GLOBAL IMPLY LTLNOT NEXT UNTIL LILAND LILOR


%start summary 
%start standaloneFC

%type <(Ast_utility.futureCond)> futureCond
%type <(Ast_utility.futureCond)> standaloneFC

%type <(Ast_utility.summary)> summary
%type <(Ast_utility.signature)> signature
%type <(Ast_utility.effect)> effect
%type <(Ast_utility.singleEffect)> singleEffect
%type <(Ast_utility.pure)> pure
%type <(Ast_utility.term)> term
%type <(Ast_utility.regularExpr)> es
%type <(Ast_utility.event)> event
%type <(Ast_utility.literal)> literal


%type <(Ast_utility.term list)> list_of_formalArgs
%type <(Ast_utility.term list)> list_of_terms
%type <(string list)> list_of_exs
%type <(string list)> exs

%type <(Ast_utility.event)>  not_event
%type <(int)> errCode




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
| NULL {Nil}
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
| l=literal {Neg l}
| UNDERLINE LPAR tLi=list_of_terms  RPAR {NegTerm (tLi)}

event:
| UNDERLINE {ANY}
| NOTSINGLE l= not_event{l }
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
| EXIST strs= list_of_exs CONCAT a = pure {Exists ( strs, a)}

futureCond: 
| s = es {[s]}
| li= futureCond CONJ s = es {li@[s]}

standaloneFC:
| fc = futureCond EOF    { fc }  

errCode:
| {0}
| SEMI i = INTE {i}

exs:
| EXIST strs= list_of_exs {strs} 
| {[]}

singleEffect: LPAR strs = exs COLON p=pure SEMI es=es SEMI fc=futureCond SEMI r=term ec=errCode RPAR {(strs, p, es, fc, r, ec)}

effect:
| s = singleEffect {[s]}
| li= effect DISJ s = singleEffect {li@[s]}

summary:
| LSPEC s=signature EQ REQUIRE p=pure  ENSURE  eff=effect   
  RSPEC {(s, p,  eff)}



