:- module(equation,[symbolic//8]).
:- set_prolog_flag(double_quotes,chars).

symbolic(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideLeft,SideRight]) -->
	expr(Fmt,Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideLeft),
	output(Fmt,arrow) xx arrow,
	expr(Fmt,CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideRight).


expr(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideH|SideT]) -->
	balanced_formula(Fmt,Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideH),
	expr_tail(Fmt,CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideT), !.

expr_tail(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[Side]) -->
	" + ",
	expr(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side).

expr_tail(_,Coeff,Coeff,Elems,Elems,Formula,Formula,[]) --> [].


balanced_formula(Fmt,[Coeff|CoeffR],CoeffR,Elems,ElemR,Formula,FormulaR,Formula) -->
	coefficient(Coeff),
	formula(Fmt,Elems,ElemR,Formula,FormulaR), !.

coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X).
coefficient(_) --> "".


%%%%% GUIDANCE FOR ERRORS --- WE HAVE TO PRETEND IT'S FROM THE FORMULA MODULE %%%%%



guidance_unparsed([],
	'The program has processed your entire equation but it is missing a required component.
	  Please ensure you are not missing any required components.

 	 The first missing component is a: '
 ).

guidance_errcode(part_first,nil,
	'You are missing a formula where it is required.

	 1. An equation has reactants and products:
	 e.g. H2 + O2 --> <H2O>, not H2 + O2 --> or --> H2O.
 	
	 2. Every plus adds another formula
	 e.g H2 + <O2> --> H2O, not H2 + --> H2O
	 
	 Please add the missing formulas.'
 ). 

guidance_errcode(part_first,punct,
	'You are missing a formula where it is required. 
	 Therefore, the highlighted symbol does not make sense here.

	 1. An equation has reactants and products:
	 e.g. H2 + O2 --> <H2O>, not H2 + O2 --> or --> H2O.
 	
	 2. Every plus adds another formula
	 e.g H2 + <O2> --> H2O, not H2 + --> H2O
	 
	 Please add the missing formulas.'
 ).


/* Let the formula parser indicate what is wrong with elements and numbers */
guidance_errcode(arrow,alpha,Message) :- formula:guidance_errcode(none,alpha,Message).
guidance_errcode(arrow,digit,Message) :- formula:guidance_errcode(none,digit,Message).

guidance_errcode(arrow,nil,
	'Chemical equations consist of reactants --> (the arrow) and products.
	 
	 1. You have forgotten to insert an arrow between the reactants and the products.
	 Find the place where the products start and insert an arrow there.

 	 2. You are entirely missing the products.
 	 Please insert the products for the equation.
 	 The program cannot figure this out for you, yet. Sorry.

 	e.g. CH4 + O2 <-->> CO2 + H2O'
).

guidance_errcode(arrow,_,
	'All operators must be properly spaced: one space before, one space after.
	 You have probably forgotten to do this for the highlighted arrow.

 	 Also, an arrow consists of: -->'
 ).


% vi: ft=prolog
