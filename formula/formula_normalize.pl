% formula_normalize.pl: The DCG rules for chemical formulas
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun



:- module(formula,[formula//5]).
%:- set_prolog_flag(double_quotes,chars).



%%% Formula parser %%%

formula(Fmt,Elems,ElemsR,Formula,FormulaR) -->
	formula_part_first(Fmt,Elems,ElemsR0,Formula,FormulaR0) xx part_first,
	(hydrate_part(Fmt,ElemsR0,ElemsR,FormulaR0,FormulaR), !).


%%% Hydrates %%%

hydrate_part(Fmt,["H","O"|ElemsR],ElemsR,[[Formula,Coeff]|SymR],SymR) --> 
	output(Fmt,dot), 
	(num_decimal(Coeff); {Coeff = 1}),
       	water_output(Fmt,Formula) xx (hydrate_h2o,group).

hydrate_part(_,Elems,Elems,Part,Part) --> [].

water_output(Fmt,[["H",2],["O",1]]) --> "H", output(Fmt,sub_start), "2", output(Fmt,sub_end),"O".


%%% Formula parts %%%

formula_part_first(Fmt,Elems,ElemsR,Part,PartR) --> 
	part(Fmt,Type,Elems,ElemsR0,Part,PartR0), 
	(formula_part(Fmt,Type,ElemsR0,ElemsR,PartR0,PartR), !).

formula_part(_,none,[],[],[],[],[],[]).
formula_part(_,multi,[],[],[],[],[],[]) :- !, fail.
formula_part(Fmt,multi,Elems,ElemsR,Part,PartR) --> part(Fmt,_,Elems,ElemsR0,Part,PartR0), (formula_part(Fmt,_,ElemsR0,ElemsR,PartR0,PartR), !).
formula_part(_,none,Elems,Elems,Part,Part) --> [].


%%% Polyatomic groups %%%

part(Fmt,multi,Elems,ElemsR,[[Sym,Num]|PartR],PartR) --> 
	"(",

	(
		group_symbol(Fmt,Elems,ElemsR,Sym) -> {true}; 
		(
			{var(Sym)} -> syntax_stop(formula:group,inside_paren)
		)
	), 

	")",
	(
		(output(Fmt,sub_start), num_decimal(Num), output(Fmt,sub_end)); 
		{var(Sym)} -> syntax_stop(formula:number)
	).

part(Fmt,multi,Elems,ElemsR,[[Sym,1]|PartR],PartR) --> group_symbol(Fmt,Elems,ElemsR,Sym).


%%% Simple element pairs %%%

part(Fmt,_,[Elem|ElemsR],ElemsR,[[Elem,Num]|PartR],PartR) --> element_symbol(Elem), (subscript(Fmt,Num), !).


%%% Subscripts %%%

subscript(_,Num) --> { nonvar(Num) -> Num = 1}, [].
subscript(Fmt,Num) --> output(Fmt,sub_start), num_decimal(Num), output(Fmt,sub_end).
subscript(_,1) --> [].



%%%%% ERROR GUIDANCE %%%%%



guidance_unparsed([],
	'Your entire formula has been processed, but you are missing a required component of a formula. Check to ensure nothing is missing.
	 The first thing the program could not find was a '
 ).


guidance_errcode(part_first,outside_paren,
	'Polyatomic ions (inside highlighted parentheses) may not be left by themselves; they must form a compound.

	 (Or be indicated correctly as such, but the program does not support that)

	 e.g. (NH4)2<SO4>, not just (NH4)2'
 ).

guidance_errcode(part_first,Type,Message) :- guidance_errcode(none,Type,Message).

guidance_errcode(number,digit,
	'The highlighted digits are not valid as a subscript or coefficient in a chemical formula.
	 Do not use an explicit 1; it is implied. Do not use a 0; you should omit that part of the formula in that case.
 	 Also, the program has a limitation on the maximum number allowed. Sorry.

 	 e.g. CH4, not C<1>H4'
 ).

guidance_errcode(number,nil,
	'A number greater than 1 is required at this point.
	 Only parenthesize polyatomic groups if there is more than one of the group.

 	e.g. (NH4)<2>, but NH4 instead of (NH4)<1>'
).

guidance_errcode(number,white,
	'A number greater than 1 is required at this point.
	 If you are entering a polyatomic group, only parenthesize it if there is more than one.

 	e.g. (NH4)<2>, but NH4 instead of (NH4)<1>'
).

guidance_errcode(number,alpha,
	'The highlighted characters are not a number.'
).

guidance_errcode(number,punct,
	'The highlighted characters are not a number.
	 (Did you accidentally press SHIFT?)'
	 ).

guidance_errcode(group,inside_paren,
	'The highlighted string is not a valid polyatomic group. 
	 Please check that you have entered it correctly. (The program may also simply not have it in its database).
 	 Also, parentheses are not necessary for single elements.

 	 e.g. (NH4)2, but not (Cl)2'
 ).

guidance_errcode(hydrate_h2o,group,
	'This program only supports hydrates: Make sure you are using water (H2O) in your hydrate part.

	 e.g. Na2CO3 . 10<H2O>'
 ).

guidance_errcode(none,punct,
	'Remove any extraneous characters from your formula.'
).

guidance_errcode(none,white,
	'Remove any extraneous spaces from your formula.'
	 
).

guidance_errcode(none,digit,
	'The number you have entered exceeds the maximum limitation in the program. Sorry.'
).

guidance_errcode(none,alpha,
	'Check that you have correctly entered the highlighted element. (The program may also simply not have it in its database).
	 Also, the highlighted characters may, in fact, be incorrect or unnecessary.

	 e.g. Nm2SO4 is actually Na2SO4'
 ).



% vi: filetype=prolog
