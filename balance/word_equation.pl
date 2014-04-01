:- module(word,[word//8]).
:- set_prolog_flag(double_quotes,chars).

word(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideLeft,SideRight]) -->
	expr(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideLeft),
	output(Fmt,arrow) xx arrow,
	expr(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideRight).


expr(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideH|SideT]) --> 
	balanced_name(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideH),
	expr_tail(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideT), !.

expr_tail(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side) --> 
	" + ",
	expr(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side).

expr_tail(Coeff,Coeff,Elems,Elems,Formula,Formula,[]) --> [].


balanced_name([Coeff|CoeffR],CoeffR,Elems,ElemR,[Formula|FormulaR],FormulaR,Formula) -->
	coefficient(Coeff),
	name(Elems,ElemR,Formula) xx fail,
	!.

coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X), " ".
coefficient(_) --> "".




%%%%% GUIDANCE FOR ERRORS  %%%%%



guidance_unparsed([],
	'The program has processed your entire equation but it is missing a required component.
	  Please ensure you are not missing any required components.

 	 The first missing component is a: '
 ).



/* Let the name parser indicate what is wrong with elements and numbers */
guidance_errcode(arrow,alpha,Message) :- name:guidance_errcode(none,alpha,Message).
guidance_errcode(arrow,digit,Message) :- name:guidance_errcode(none,digit,Message).

guidance_errcode(arrow,nil,
	'Chemical equations consist of reactants --> (the arrow) and products.
	 
	 1. You have forgotten to insert an --> between the reactants and the products.
 	 Find the place where the products start and insert an --> there.

 	 2. You are entirely missing the products.
 	 Please insert the products for the equation.
 	 The program cannot figure this out for you, yet. Sorry.

 	e.g. methane + oxygen <-->> carbon dioxide + water'
).

guidance_errcode(arrow,_,
	'All operators must be properly spaced: one space before, one space after.
	 
	 Possible mistakes:
	 1. You have forgotten to correctly space the highlighted --> or +
	 2. You are missing a required operator at the highlighted position
	 3. You have entered spurious characters instead of a valid operator

 	 Also, an arrow consists of: -->'
 ).

/* Let the name parser indicate what is wrong with elements and numbers */
guidance_errcode(none,alpha,Message) :- name:guidance_errcode(none,alpha,Message).
guidance_errcode(none,digit,Message) :- name:guidance_errcode(none,digit,Message).

guidance_errcode(none,white,
	'You have already entered all of the required components of an equation.
	 Therefore, the program does not expect the highlighted tokens to appear.

 	 Either you have entered spurious characters; in which case, you should remove them,
	 or you are missing/misentered a +, in which case you should correct it.
 	
 	 NOTE: 1 space before, 1 space after an --> or +'
 ).

guidance_errcode(none,punct,Message) :- guidance_errcode(none,white,Message).

/* Let the name parser indicate what is wrong with elements and numbers */
guidance_errcode(fail,alpha,Message) :- name:guidance_errcode(fail,alpha,Message).
guidance_errcode(fail,digit,Message) :- name:guidance_errcode(fail,digit,Message).



guidance_errcode(fail,nil,
	'You are missing a compound where it is required.

	 1. An equation has reactants and products:
	 e.g. hydrogen + oxygen --> <water>, not hydrogen + oxygen --> or --> water.
 	
	 2. Every plus adds another compound name:
	 e.g hydrogen + <oxygen> --> water, not hydrogen + --> water
	 
	 Please add the missing compounds.'
 ). 

guidance_errcode(fail,punct,
	'You are missing a compound where it is required. 
	 Therefore, the highlighted symbol does not make sense here.

	 1. An equation has reactants and products:
	 e.g. hydrogen + oxygen --> <water>, not hydrogen + oxygen --> or --> water.
 	
	 2. Every plus adds another compound name:
	 e.g hydrogen + <oxygen> --> water, not hydrogen + --> water
	 
	 Please add the missing compounds.'
 ).

guidance_errcode(fail,white,Message) :- guidance_errcode(fail,punct,Message).




%%%%% GUIDANCE FOR NAME PARSING ERRORS SPECIFIC TO EQUATIONS %%%%%



:- multifile ionic:guidance_errcode/3.

ionic:guidance_errcode(nonmetal,punct,
	'You have entered the following extraneous characters instead of a valid non-metal ide.
	 If an operator is highlighted, you have forgotten to enter the second part of the compound name

	 Ionic compounds are in the form metal non-metalide.'
 ).

% vi: ft=prolog
