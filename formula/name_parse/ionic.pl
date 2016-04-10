% ionic.pl: DCG rules for the names of binary ionic compounds (hydrates supported) and the names of acids
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2016 Nicholas Paun



:- module(ionic,[ionic//3]).
:- set_prolog_flag(double_quotes,chars).
:- use_module(acid).
:- use_module(covalent,[sub_general//2]).



%%% Different algorithm is used depending on convertion from name to formula or from formula to name %%% 

ionic(Elems,ElemsR,Formula) --> {var(Formula)}, fwd(Elems,ElemsR,Formula), !.
ionic(_,_,Formula) --> {nonvar(Formula)}, rev(Formula).


%%% The Algorithms %%% 

fwd(MElems,FinalRest,[Anion,Cation|Appended]) --> 
	compound(MElems,FinalRest,ChargeFormula,Appended),
	{fwd_algo([Anion,Cation],ChargeFormula)}.

fwd_algo(FFormula,[MSym,MCharge,NMSym,NMCharge]) :-
		GCD is gcd(MCharge,NMCharge),
		MSub is abs(NMCharge / GCD),
		NMSub is abs(MCharge / GCD),
		Formula = [[MSym,MSub],[NMSym,NMSub]],
		acid_correct_hydrogen(Formula,FFormula).



rev(Formula) -->
	{Formula = [[_,MSub],[_,NMSub]|Hydrate]},
	{rev_algo(Formula,ChargeFormula)},
	compound(_,_,ChargeFormula,Hydrate),
	!,

	/* Corrector: remove if unecessary */
	(
		{GCD is gcd(MSub,NMSub)}, 
		{GCD = 1, !; throw(error(logic_error(ionic:corrector_not_reduced,Formula),_))}
	).

rev_algo(Formula,[MSym,MCharge,NMSym,NMCharge]) :-
		Formula = [[TestMSym,_]|_Rest],
		FFormula = [[MSym,MSub],[NMSym,NMSub]|_Hydrate],
		(TestMSym = "H" -> 
			acid_correct_hydrogen(Formula,FFormula);
			(Formula = FFormula, charge_check(metal,MSym,MCharges))	
		),

		(charge(NMSym,NMCharge), !),

		NMTotal is abs(NMSub * NMCharge),
		MCharge is NMTotal / MSub,

		/* Corrector: remove if unnecessary */
	
		(
			(is_list(MCharges) -> member(MCharge,MCharges); MCharge = MCharges), !; 
			throw(error(logic_error(ionic:corrector_invalid_subscript,Formula),_))
		).


%%% Ionic Compound Naming Rules %%% 

compound(Elems,Rest,Formula,[]) --> acid(Elems,Rest,Formula).
compound(MElems,FinalRest,[MSym,MCharge,NMSym,NMCharge],Hydrate) -->
	cation(MElems,MRest,MSym,MCharge), 
	" " xx anion_part, 
	anion(MRest,NMRest,NMSym,NMCharge),
	hydrate_part(NMRest,FinalRest,Hydrate).

cation(Elems,Rest,Formula,Charge) --> group(Elems,Rest,Formula,_),
	{charge_check(metal,Formula,Charge)} xx (cation, mark).

cation([Sym|Rest],Rest,Sym,Charge) --> metal(Sym,Charge).

anion(Elems,Rest,Formula,Charge) --> group(Elems,Rest,Formula,_), 
		{charge_check(nonmetal,Formula,Charge)} xx (anion, mark).

anion([Sym|Rest],Rest,Sym,Charge) --> nonmetal_ide(Sym,_,Charge) xx nonmetal, 
	{Charge < 0} xx noble_gas_q.


%%% Hydrates %%%

hydrate_part(["H","O"|ElemR],ElemR,[[[["H",2],["O",1]],Num]]) --> 
	" ", 
	(sub_general(Num,Suffix), Suffix), 
	"hydrate" xx hydrate_h2o.

hydrate_part(Pass,Pass,[]) --> [].

hydrate_part([],[],Formula,[],[]) :- throw(error(logic_error(ionic:corrector_input_too_long,Formula),_)).

%%% Metals %%% 

metal(Sym,Charge) --> element(Sym,_), {charge(Sym,Charges)}, metal_valence(Charge,Charges), !.

metal_valence(Charge,Charges) --> 
	{is_list(Charges)},
	multivalent_charge(Charge) xx charge,
	{member(Charge,Charges)} xx charge_invalid.

metal_valence(Charge,Charges) --> 
	{Charges > 0},
       	multivalent_corrector(Charge), /* CORRECTOR: remove if unecessary */	
	{Charge = Charges}.

/* CORRECTOR: remove if unnecessary */
multivalent_corrector(Charge) --> {var(Charge)} -> 
	( 
		(
			multivalent_charge(_), 
			syntax_stop(ionic:corrector_not_multivalent)
		); 
		{true}
	); {true}.


%%% Multivalent Charges %%% 

multivalent_charge(Charge) --> "(", num_roman(Charge), ")".

num_roman(8) --> "VIII".
num_roman(8) --> "IIX".

num_roman(7) --> "VII".
num_roman(6) --> "VI".
num_roman(5) --> "V".
num_roman(4) --> "IV".
num_roman(3) --> "III".
num_roman(2) --> "II".
num_roman(1) --> "I".




%%%%% ERROR MESSAGE GUIDANCE %%%%% 



guidance_unparsed([],
	'The program has processed your entire chemical name and has found it to be an ionic compound.
	 You are however, missing a required component of an ionic compound name.
 	 Please check to ensure that there is nothing missing.

 	 The first missing component is a: '
 ).

guidance_errcode(charge,outside_paren,
	'The ionic charge you have entered (inside the highlighted parentheses) has been entered incorrectly.
	 You must use capital roman numerals in parentheses.

	e.g copper<(II)>, not something like copper<(2)> or copper<(ii)>.'
 ).

guidance_errcode(charge,digit,
	'Ionic charges must be entered with parenthesized roman numerals.
	
	 e.g. copper<(II)> instead of copper<2>'
 ).

guidance_errcode(charge,alpha,
	'Ionic charges must be in parentheses ().'
).

guidance_errcode(charge,white,
	'The element you have entered is multivalent. You must provide the charge in the highlighted space.
	 NOTE: Use capital roman numerals in parenthesis

	 e.g. (II)'
 ).

guidance_errcode(charge_invalid,white,
 	'The ionic charge you have entered (left of highlighting) is not valid for the given element. Please re-check.
	 NOTE: It is very likely that the program is simply missing this charge from its database'
 ).

guidance_errcode(charge_invalid,nil,
	'A multivalent metal ion (probably with an incorrect charge) is not valid on its own.
	 Ionic compounds consist of a positive ion (e.g. a metal) and a negative ion (e.g. a non-metal with an -ide ending).

 	 e.g copper(II) <chloride>, but not just copper(II)'
).


guidance_errcode(corrector_not_multivalent,white,
	'BUSTED! You have entered an ionic charge (left of highlighting) for an element that is not multivalent.
	 
	 e.g. sodium chloride, not sodium(I) chloride'
 ).

guidance_errcode(corrector_not_multivalent,_,
	'BUSTED! You have entered an ionic charge (left of highlighting) for an element that is not multivalent.
	 Not only that, you have also inserted some incorrect characters (the highlighted part).
	 
	 e.g. sodium chloride, not sodium(I) chloride'
 ).

guidance_errcode(anion,_,
	'The polyatomic ion you have entered (ending at the highlighted mark) is a positive ion.
	 A negative ion is expected here.
	 Ionic compounds consist of a positive ion and a negative ion.

 	e.g sodium <acetate> is correct, but not sodium <ammonium>'
).

guidance_errcode(cation,_,
	'The polyatomic ion you have entered (ending at the highlighted mark) is a negative ion.
	 A positive ion is expected here.
	 Ionic compounds consist of a positive ion and a negative ion.

 	e.g <ammonium> chloride is correct, but not <dihydrogen phosphate> chloride'
).

guidance_errcode(hydrate_h2o,alpha,
	'Only hydrates (water; . nH2O; etc.) are supported by the program.').

guidance_errcode(hydrate_h2o,nil,
	'You have left a number prefix by itself. What are we counting?
	 (The answer is "hydrate").

	 e.g copper(II) chloride tetra<hydrate>, not copper(II) chloride tetra'
 ).

guidance_errcode(hydrate_h2o,_,
	'Remove the highlighted incorrect characters.').

guidance_errcode(noble_gas_q,_,
	'The program has no idea what ionic compounds form from noble gases.
	 Your chemistry is too fancy for us. Sorry.'
 ).

guidance_errcode(anion_part,nil,
	'Your compound is missing a non-metal part, yet you are entering an ionic compound.
	 Ionic compounds consist of a positive ion (e.g. a metal) and a negative ion (e.g. a non-metal with an -ide ending).

 	 e.g copper(II) <chloride>, but not just copper(II)'
 ).

%%% Guidance for logic errors not relating to grammatical mistakes %%%

guidance_general(corrector_not_reduced,
	'BUSTED! The formula you have entered is not reduced to simplest terms.
	 All ionic formulas are empirical formulas, the lowest possible ratio of the elements.

 	 Always remember to simplify the formula.

 	 e.g Pb2O4 must be reduced to PbO2.
	 
	 The following formula is incorrectly reduced: '
 ).

guidance_general(corrector_invalid_subscript,
	'BUSTED! The subscripts of the formula you have entered are invalid.
	 Your input was recognized as an ionic compound, because it consists of a positive and a negative ion, 
	 but the subscripts you have entered do not correspond with the correct charges of these ions.
	
	 e.g. NaSO4 is incorrect because the charge of the SO4 ion is 2- and the charge of the Na ion is 1+, so the correct formula is Na2SO4.

	 Please verify that you have correctly balanced the charges of the positive and negative ions, by using appropriate subscripts.
	 
	 The following formula containes invalid subscripts: '
).


guidance_general(corrector_input_too_long,
	'BUSTED! The formula you have entered is too long. Ionic compounds consist of a cation, an anion, and optionally a hydrate.
	 Both the cation and the anion were processed correctly, but the rest of your input is invalid.').

%%%%% CATCH-ALL ERROR GUIDANCE %%%%%

guidance_errcode(ErrCode,Type,Message) :- name:guidance_errcode(ErrCode,Type,Message). % Let the main file handle anything we can't figure out
