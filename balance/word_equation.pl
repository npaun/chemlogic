% word_equation.pl: The DCG for word chemical equations
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(word,[word//9]).
:- set_prolog_flag(double_quotes,chars).




word(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[ElemsL,ElemR0],[SideLeft,SideRight]) -->
	expr(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideLeft),
	{var(ElemR0) -> make_element_sideset(Elems-ElemR0,ElemsL-[]); true},
	output(Fmt,arrow) xx arrow,
	products(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideRight).

products(_,Coeff,Coeff,Elems,Elems,Formula,Formula,_,[],[]).
products(Fmt,Coeff,CoeffR,Elems,ElemsR,Formula,FormulaR,SideRight) --> 
	" " xx arrow_space, 
	expr(Fmt,Coeff,CoeffR,Elems,ElemsR,Formula,FormulaR,SideRight).

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

make_element_sideset(Elems,ElemsCopy) :-
	copy_term(Elems,ElemsCopy).



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
	'Complete chemical equations consist of reactants --> (the arrow) and products.
	 You may also leave out the products, if you would like the program to complete the reaction.


	 1. You have forgotten to insert an --> between the reactants and the products.
 	 Find the place where the products start and insert an --> there.
	 e.g. methane + oxygen <-->> carbon dioxide + water

         2. You have left out the arrow, when requesting reaction completion.
	 Place an arrow at the end of the reactants to indicate that you wish to determine the products.
	 (Do not place a trailing space after the arrow, in this case only.)
	 e.g. hydrochloric acid + sodium hydroxide <-->>'

).

guidance_errcode(arrow,_,
	'All operators must be properly spaced: 1 space before, 1 space after.
	 
	 Possible mistakes:
	 1. You have forgotten to correctly space the highlighted --> or +
	 2. You are missing a required operator at the highlighted position
	 3. You have entered incorrect characters instead of a valid operator

 	 Also, an arrow consists of: -->'
 ).

/* Let the name parser indicate what is wrong with elements and numbers */
guidance_errcode(none,alpha,Message) :- name:guidance_errcode(none,alpha,Message).
guidance_errcode(none,digit,Message) :- name:guidance_errcode(none,digit,Message).

guidance_errcode(none,white,
	'You have already entered all of the required components of an equation.
	 Therefore, the program does not expect the highlighted component to appear.

 	 Either you have entered unnecessary characters; in which case, you should remove them,
	 or you are missing/misentered a +, in which case you should correct it.
 	
 	 NOTE: 1 space before, 1 space after an --> or +'
 ).

guidance_errcode(none,punct,Message) :- guidance_errcode(none,white,Message).

/* Let the name parser indicate what is wrong with elements and numbers */
guidance_errcode(fail,alpha,Message) :- name:guidance_errcode(fail,alpha,Message).
guidance_errcode(fail,digit,Message) :- name:guidance_errcode(fail,digit,Message).



guidance_errcode(fail,nil,
	'You are missing a chemical name where it is required.

	 1. A complete chemical equation has both reactants and products:
	 e.g. hydrogen + oxygen --> <water>, not --> water.
 

	 2. If only the reactants are provided, the program will attempt to complete the equation.
	 NOTE: In this case, an arrow must be placed at the end of the reactants.
	 e.g. acetic acid + zinc hydroxide -->

	 3. Every plus adds another chemical name:
	 e.g hydrogen + <oxygen> --> water, not hydrogen + --> water
	 
	 Please add the missing chemical names.'
 ). 

guidance_errcode(fail,punct,
	'You are missing a chemical name where it is required. 
	 Therefore, the highlighted symbol does not make sense here.

	 1. A complete chemical equation has reactants and products:
	 e.g. hydrogen + oxygen --> <water>, not hydrogen + oxygen --> or --> water.
 
	 2. If only the reactants are provided, the program will attempt to complete the equation.
	 NOTE: In this case, an arrow must be placed at the end of the reactants.
	 e.g. acetic acid + zinc hydroxide -->

	 3. Every plus adds another chemical name:
	 e.g hydrogen + <oxygen> --> water, not hydrogen + --> water
	

	 Please add the missing chemical names.'
 ).

guidance_errcode(fail,white,Message) :- guidance_errcode(fail,punct,Message).




%%%%% GUIDANCE FOR NAME PARSING ERRORS SPECIFIC TO EQUATIONS %%%%%



:- multifile ionic:guidance_errcode/3.

ionic:guidance_errcode(nonmetal,punct,
	'You have entered the following incorrect characters instead of a valid non-metal with an -ide ending.
	 If an operator is highlighted, you have forgotten to enter the second part of the compound name

	 Ionic compounds consist of a positive ion (e.g. a metal) and a negative ion (e.g. a non-metal with an -ide ending)
	 e.g. sodium chloride'
 ).

% vi: ft=prolog
