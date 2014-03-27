:- module(ionic,[ionic//3,acid_base//1]).
:- set_prolog_flag(double_quotes,chars).
:- use_module(oxyanion).
:- use_module(covalent,[sub_general//2]).



%%% Different algorithm is used depending on convertion from name to formula or from formula to name %%% 

ionic(Elems,ElemsR,Formula) --> {var(Formula)}, fwd(Elems,ElemsR,Formula), !.
ionic(_,_,Formula) --> {nonvar(Formula)}, rev(Formula).


%%% The Algorithms %%% 

fwd(MElems,FinalRest,[[MSym,MSub],[NMSym,NMSub]|Appended]) --> compound(MElems,FinalRest,[MSym,MCharge,NMSym,NMCharge],Appended),
	{
		GCD is gcd(MCharge,NMCharge),
		MSub is abs(NMCharge / GCD),
		NMSub is abs(MCharge / GCD)
	}.

rev([[MSym,MSub],[NMSym,NMSub]|Appended]) -->
	{
		%  TODO: If the metal is monovalent, don't bother conjuring it up! Just use it.
		%  Also, no need to actually check these charges, is there?
	
		(MSym = "H"; charge_check(metal,MSym)),

		(charge(NMSym,NMCharge), !),

		NMTotal is abs(NMSub * NMCharge),
		MCharge is NMTotal / MSub
	},

	compound(_,_,[MSym,MCharge,NMSym,NMCharge],Appended),
	!,

	/* Corrector: remove if unecessary */
	(
		{GCD is gcd(MSub,NMSub)}, 
		{GCD = 1} xx corrector_not_reduced
	).


%%% Ionic Compound Naming Rules %%% 

compound(Elems,Rest,Formula,[]) --> acid(Elems,Rest,Formula).
compound(MElems,FinalRest,[MSym,MCharge,NMSym,NMCharge],Hydrate) --> 
	cation(MElems,MRest,MSym,MCharge), 
	" " xx anion_part, 
	anion(MRest,NMRest,NMSym,NMCharge), 
	hydrate_part(NMRest,FinalRest,Hydrate).

cation(Elems,Rest,Formula,Charge) --> group(Elems,Rest,Formula,_),
	{charge_check(metal,Formula,Charge)} xx cation.

cation([Sym|Rest],Rest,Sym,Charge) --> metal(Sym,Charge).

anion(Elems,Rest,Formula,Charge) --> group(Elems,Rest,Formula,_), 
		{charge_check(nonmetal,Formula,Charge)} xx anion.

anion([Sym|Rest],Rest,Sym,Charge) --> nonmetal_ide(Sym,_,Charge) xx nonmetal, 
	{Charge < 0} xx noble_gas_q.


%%% Hydrates %%%

hydrate_part(["H","O"|ElemR],ElemR,[[[["H",2],["O",1]],Num]]) --> 
	" ", 
	(sub_general(Num,Suffix), Suffix), 
	"hydrate" xx hydrate_h2o.

hydrate_part(Pass,Pass,[]) --> [].


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

%%% Acids %%% 

acid(["H"|Elems],Rest,["H",1,ASym,ACharge]) --> acid_anion(Elems,Rest,ASym,ACharge), " acid" xx acid.

acid_anion(Elems,Rest,ASym,ACharge) --> hydro_acid(Elems,Rest,ASym,ACharge).
acid_anion(Elems,Rest,ASym,ACharge) --> oxyanion_acid(Elems,Rest,ASym,ACharge).
acid_anion(Elems,Rest,ASym,ACharge) --> polyatomic_oxy_acid(Elems,Rest,ASym,ACharge).
acid_anion(Elems,Rest,ASym,ACharge) --> polyatomic_hydro_acid(Elems,Rest,ASym,ACharge).

hydro_acid([ASym|Rest],Rest,ASym,ACharge) --> "hydro", acid_base(ASym), ic_suffix,
		{charge_check(nonmetal,ASym,ACharge)} xx nonmetal_acid. 

polyatomic_oxy_acid(Elems,Rest,ASym,ACharge) --> group_base(Elems,Rest,ASym,_), ic_suffix,
	(
	
		{member(["O",_],ASym), !} ; 
		({var(Elems)} -> syntax_stop(ionic:oxy_acid_rule))
		
	), 
		{charge_check(nonmetal,ASym,ACharge)} xx nonmetal_acid.


polyatomic_hydro_acid(Elems,Rest,ASym,ACharge) --> "hydro", group_base(Elems,Rest,ASym,_), ic_suffix, 
	 
		{ \+ member(["O",_],ASym), ! } xx hydro_acid_rule,
		{charge_check(nonmetal,ASym,ACharge)} xx nonnmetal_acid.

acid_base(Sym) --> element_base(Sym,_), 
	acid_ion_suffix(Sym) xx acid_suffix.

ic_suffix --> "ic" xx ic_acid_suffix.


%%%%% ERROR MESSAGE GUIDANCE %%%%% 



guidance_unparsed([],
	'The program has processed your entire chemical name and has found it to be an ionic compound.
	 You are however, missing a required component of an ionic compound name.
 	 Please check to ensure that there is nothing missing.

 	 The first missing component is a: '
 ).

guidance_errcode(charge,outside_paren,
	'The ionic charge you have entered (inside the highlighted parentheses) is malformed. You must use capital roman numerals in parentheses.

	e.g copper<(II)>, not something like copper<(2)> or copper<(ii)>.'
 ).

guidance_errcode(charge,digit,
	'Ionic charges must be entered with parenthesized roman numerals.
	
	 e.g. copper<(II)> instead of copper<2>'
 ).

guidance_errcode(charge,alpha,
	'Ionic charges must be in parenthesis ().'
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

/* Appears to have no effect */
guidance_errcode(charge_invalid,nil,
	'A positive ion (written with a valence charge) is not valid on its own.
	 Ionic compounds consist of a metal and a nonmetalide.

 	 e.g copper(II) <chloride>, but not just copper(II)'
).


guidance_errcode(corrector_not_multivalent,white,
	'BUSTED! You have entered an ionic charge (left of highlighting) for an element that is not multivalent.
	 
	 e.g. sodium chloride, not sodium(I) chloride'
 ).

guidance_errcode(corrector_not_multivalent,_,
	'BUSTED! You have entered an ionic charge (left of highlighting) for an element that is not multivalent.
	 Not only that, you have also inserted some extraneous characters (the highlighted part).
	 
	 e.g. sodium chloride, not sodium(I) chloride'
 ).

guidance_errcode(anion,alpha,
	'The polyatomic ion you have entered is a positive ion. A negative ion is expected here.
	 Ionic compounds consist of a positive ion and a negative ion.

 	e.g sodium <acetate> is correct, but not sodium <ammonium>'
).

guidance_errcode(cation,alpha,
	'The polyatomic ion you have entered is a negative ion. A positive ion is expected here.
	 Ionic compounds consist of a positive ion and a negative ion.

 	e.g <ammonium> chloride is correct, but not <dihydrogen phosphate> chloride'
).

guidance_errcode(corrector_not_reduced,_,
	'BUSTED! The formula you have entered is not reduced to simplest terms.
	 All ionic formulas are empirical formulas, the lowest possible ratio of the elements.

 	 Always remember to simplify the formula.

 	 e.g Pb2O4 must be reduced to PbO2.'
 ).

guidance_errcode(hydrate_h2o,alpha,
	'Only hydrates (water; . nH2O; etc.) are supported by the program.').

guidance_errcode(hydrate_h2o,nil,
	'You have left a number prefix by itself. What are we counting?
	 (The answer is "hydrate").

	 e.g copper(II) chloride tetra<hydrate>, not copper(II) chloride tetra'
 ).

guidance_errcode(hydrate_h2o,_,
	'Remove the highlighted extraneous characters.').

guidance_errcode(noble_gas_q,_,
	'The program has no idea what ionic compounds form from noble gases.
	 Your chemistry is too fancy for us. Sorry.'
 ).

guidance_errcode(anion_part,nil,
	'Your compound is missing a non-metal part, yet you are entering an ionic compound.
	 Ionic compounds are in the form metal non-metalide.

	  e.g copper(II)< chloride>, not just copper(II) on its own'
 ).



%%%%% ACID ERROR MESSAGE GUIDANCE %%%%%



guidance_errcode(acid_suffix,_,
	'When naming acids, sulfur and phosphorus use a different name than their ion.
	 (Please ignore the highlighting)

	 sulfur makes sulfide and sulf<ur>... acids
 	 phosphorus makes phosphide as phosph<or>... acids'
 ).

guidance_errcode(ic_acid_suffix,_,
	'Acids of this type must have names ending in "ic".

	 1. Acids not containing oxygen (starting with hydro):
	 e.g. hydrosulfur<ic> acid, but not hydrosulfur<ous> acid

	 2. Acids containing oxygen, but not an oxyanion:
	 e.g. acet<ic> acid, but not acet<ous> acid'
 ).

guidance_errcode(nonmetal_acid,alpha,
	'A non-metal (or a negative polyatomic ion) is expected here.
	 All acids are covalent compounds consisting of hydrogen and an anion. 
 	 Their formulas all follow ionic rules (empirical formula based on charges).
	 Acids are named according to special rules.

 	 e.g. You cannot create hydrocupric acid (H2Cu), but you can create hydrosulfuric acid (H2S) and nitric acid (HNO3).'
 ). 

guidance_errcode(nonmetal_acid,nil,
	'A non-metal (or a negative polyatomic ion) is expected here.
	 All acids are covalent compounds consisting of hydrogen and an anion.
	 (This is just the most likely situation --- you are also missing the suffix " acid", 
	  so you could be making a different mistake.)

 	 e.g. You cannot create hydrocupric acid (H2Cu), but you can create hydrosulfuric acid (H2S) and nitric acid (HNO3).'
 ).

guidance_errcode(nonmetal_acid,white,
	'A non-metal (or a negative polyatomic ion) is expected here.
	 All acids are covalent compounds consisting of hydrogen and an anion.
	 (This is just the most likely situation --- you may be entering something other than an acid,
	  making a different mistake.) 

 	 e.g. You cannot create hydrocupric acid (H2Cu), but you can create hydrosulfuric acid (H2S) and nitric acid (HNO3).'
 ).

guidance_errcode(oxy_acid_rule,_,
	'Acids not containing oxygen (like the one you are entering) must be prefixed with hydro-.
	
	 e.g. HCN is <hydro>cyanic acid, not cyanic acid.'
 ).



guidance_errcode(hydro_acid_rule,_,
	'Acids containing oxygen (like the one you are entering) are not prefixed with hydro-.

	e.g. HCH3C<OO> is acetic acid, not <hydro>acetic acid.'
).

guidance_errcode(acid,nil,
	'All acids end with the suffix " acid", but you have forgotten to insert it.'
).


guidance_errcode(acid,white,Message) :- guidance_errcode(acid,nil,Message).


guidance_errcode(acid,_,
	'All acids end the suffix " acid", not whatever you entered.'
).



%%%%% CATCH-ALL ERROR GUIDANCE %%%%%



guidance_errcode(ErrCode,Type,Message) :- name_parse:guidance_errcode(ErrCode,Type,Message). % Let the main file handle anything we can't figure out
