% symbolic_equation.pl: The DCG for symbolic chemical equations
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(symbolic,[symbolic//12]).
:- set_prolog_flag(double_quotes,chars).




symbolic(Fmt,Coeff,CoeffR,Elems,ElemsR,Formula,FormulaR,[ElemsL,ElemsR0],[SideLeft,SideRight],Stoich,Qty,QtyR) -->
	expr(Fmt,Coeff,CoeffR0,Elems,ElemsR0,Formula,FormulaR0,SideLeft,Stoich,Qty,QtyR0),
	{var(ElemsR0) -> make_element_sideset(Elems-ElemsR0,ElemsL-[]); true},
	output(Fmt,arrow) xx arrow,
	products(Fmt,CoeffR0,CoeffR,ElemsR0,ElemsR,FormulaR0,FormulaR,SideRight,Stoich,QtyR0,QtyR).

products(_,Coeff,Coeff,Elems,Elems,Formula,Formula,_,_,Qty,Qty,[],[]).

products(Fmt,Coeff,CoeffR,Elems,ElemsR,Formula,FormulaR,SideRight,Stoich,Qty,QtyR) --> 
	" " xx arrow_space, 
	expr(Fmt,Coeff,CoeffR,Elems,ElemsR,Formula,FormulaR,SideRight,Stoich,Qty,QtyR).

expr(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideH|SideT],Stoich,Qty,QtyR) -->
	balanced_formula(Fmt,Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideH,Stoich,Qty,QtyR0),
	expr_tail(Fmt,CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideT,Stoich,QtyR0,QtyR), !.

expr_tail(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side,Stoich,Qty,QtyR) -->
	" + ",
	expr(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side,Stoich,Qty,QtyR).

expr_tail(_,Coeff,Coeff,Elems,Elems,Formula,Formula,[],_,Qty,Qty) --> [].


balanced_formula(Fmt,[Coeff|CoeffR],CoeffR,Elems,ElemR,[Formula|FormulaR],FormulaR,Formula,stoich,[Qty|QtyR],QtyR) -->
	quantity_prefix(Qty),
	coefficient(Coeff),
	formula(Fmt,Elems,ElemR,Formula,[]), !.

balanced_formula(Fmt,[Coeff|CoeffR],CoeffR,Elems,ElemR,[Formula|FormulaR],FormulaR,Formula,_,Qty,Qty) -->
	coefficient(Coeff),
	formula(Fmt,Elems,ElemR,Formula,[]), !.


coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X).
coefficient(_) --> "".

make_element_sideset(Elems,ElemsCopy) :-
	copy_term(Elems,ElemsCopy).


%%%%% GUIDANCE FOR ERRORS  %%%%%



guidance_unparsed([],
	'The program has processed your entire equation but it is missing a required component.
	  Please ensure you are not missing any required components.

 	 The first missing component is a: '
 ).



/* Let the formula parser indicate what is wrong with elements and numbers */
guidance_errcode(arrow,alpha,Message) :- formula:guidance_errcode(none,alpha,Message).
guidance_errcode(arrow,digit,Message) :- formula:guidance_errcode(none,digit,Message).

guidance_errcode(arrow,nil,
	'Complete chemical equations consist of reactants --> (the arrow) and products.
	 You may also leave out the products, if you would like the program to complete the reaction.
	 
	 1. You have forgotten to insert an --> between the reactants and the products.
	 Find the place where the products start and insert an --> there.
 	 
	 e.g. CH4 + O2 <-->> CO2 + H2O

 	 2. You have left out the arrow, when requesting reaction completion.
	 Place an arrow at the end of the reactants to indicate that you wish to determine the products.
	 (Do not place a trailing space after the arrow, in this case only.)

	 e.g. HCl + NaOH <-->>'
).

guidance_errcode(arrow,_,
	'All operators must be properly spaced: 1 space before, 1 space after.
	 1. You have forgotten to correctly space the highlighted --> or +
	 2. You are missing a required operator at the highlighted position

 	 Also, an arrow consists of: -->'
 ).

guidance_errcode(arrow_space,_,
	'You are missing a required space after the arrow.
	 All operators must be properly spaced: 1 space before, 1 space after.

	 Please insert a space before the highlighted component.'
).

/* Let the formula parser indicate what is wrong with elements and numbers */
guidance_errcode(none,alpha,Message) :- formula:guidance_errcode(none,alpha,Message).
guidance_errcode(none,digit,Message) :- formula:guidance_errcode(none,digit,Message).

guidance_errcode(none,white,
	'You have already entered all of the required components of an equation.
	 Therefore, the program does not expect the highlighted compontent to appear.

 	 Either you have entered unnecessary characters; in which case, you should remove them,
	 or you are missing/misentered a +, in which case you should correct it.

	 NOTE: 1 space before, 1 space after an --> or +'
 ).

guidance_errcode(none,punct,Message) :- guidance_errcode(none,white,Message).



%%%%% GUIDANCE FOR FORMULA ERRORS SPECIFIC TO EQUATIONS %%%%%



:- multifile formula:guidance_errcode/3.

formula:guidance_errcode(part_first,nil,
	'You are missing a formula where it is required.

	 1. A complete equation has both reactants and products:
	 e.g. H2 + O2 --> <H2O>, not --> H2O.
 	
	 2. If only the reactants are provided, the program will attempt to complete the equation.
	 NOTE: In this case, an arrow must be placed at the end of the reactants.
	 e.g. HCH3COO + Zn(OH)2 -->

	 3. Every plus adds another formula
	 e.g H2 + <O2> --> H2O, not H2 + --> H2O
	 
	 Please add the missing formulas.'
 ). 

formula:guidance_errcode(part_first,punct,
	'You are missing a formula where it is required. 
	 Therefore, the highlighted symbol does not make sense here.

	 1. A complete chemical equation has both reactants and products:
	 e.g. H2 + O2 --> <H2O>, not --> H2O.
 	
	 2. If only the reactants are provided, the program will attempt to complete the equation.
	 NOTE: In this case, an arrow must be placed at the end of the reactants.
	 e.g. HCHCOOH + Zn(OH)2 -->

	 3. Every plus adds another formula:
	 e.g H2 + <O2> --> H2O, not H2 + --> H2O

	 
	 Please add the missing formulas.'
 ).

formula:guidance_errcode(part_first,white,Message) :- formula:guidance_errcode(part_first,punct,Message).

% vi: ft=prolog
