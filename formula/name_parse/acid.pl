% acid.pl: DCG rules for the names of acids. Used by the ionic compound handling functions.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(acid,[acid//3,acid_base//1,acid_correct_hydrogen/2]).
:- use_module(oxyanion).
:- set_prolog_flag(double_quotes,chars).



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
		({var(ACharge)} -> syntax_stop(ionic:oxy_acid_rule))

	),
		{charge_check(nonmetal,ASym,ACharge)} xx nonmetal_acid.


polyatomic_hydro_acid(Elems,Rest,ASym,ACharge) --> "hydro", group_base(Elems,Rest,ASym,_), ic_suffix,

		{ \+ member(["O",_],ASym), ! } xx hydro_acid_rule,
		{charge_check(nonmetal,ASym,ACharge)} xx nonnmetal_acid.

acid_base(Sym) --> element_base(Sym,_),
	acid_ion_suffix(Sym) xx acid_suffix.

ic_suffix --> "ic" xx ic_acid_suffix.


%%% Correct acid formulas %%%

/* NOTE:
 **** The purpose of this correction  ****
Hydrogen-containing ions (beginning with "bi" or "hydrogen") can react to form acids.
Because acids can be conceptualized as an ionic compound consisting of hydrogen and an anion, one might decide to create an acid consisting of hydrogen and bicarbonate -- "bicarbonic acid", for example. The formula of this acid would be HHCO3, which is not inccorect, but should rather be stated as "H2CO3".

This mistake might be made by a student, or by Chemlogic itself, when it re-arranges ions to complete double-replacement reactions.
Since the program may make this mistake and since it is not entirely incorrect, we will just silently fix it.
Noticing the correction might help the user learn the correct way to write these formulas.
*/


acid_correct_hydrogen([["H",HSub],[[["H",HASub]|ASym],ASub]],[["H",HTotalSub],[ASym,ASub]]) :- HTotalSub is HSub + HASub, !.
acid_correct_hydrogen(Formula,Formula).

%%%%% ACID ERROR MESSAGE GUIDANCE %%%%%



guidance_unparsed([],
	'The program has processed your entire chemical name and has found it to be an ionic compound.
	 You are however, missing a required component of an ionic compound name.
 	 Please check to ensure that there is nothing missing.

 	 The first missing component is a: '
 ).


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
	 (This is just the most likely situation --- you are also missing the suffix " acid", so you could be making a different mistake.)

 	 e.g. You cannot create hydrocupric acid (H2Cu), but you can create hydrosulfuric acid (H2S) and nitric acid (HNO3).'
 ).

guidance_errcode(nonmetal_acid,white,
	'A non-metal (or a negative polyatomic ion) is expected here.
	 All acids are covalent compounds consisting of hydrogen and an anion.
	 (This is just the most likely situation --- you may be entering something other than an acid, making a different mistake.)

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
	'All acids end with  -acid, but you have forgotten to insert it.'
).


guidance_errcode(acid,white,Message) :- guidance_errcode(acid,nil,Message).


guidance_errcode(acid,_,
	'All acids end with  -acid, not whatever you entered.'
).



%%%%% CATCH-ALL ERROR GUIDANCE %%%%%



guidance_errcode(ErrCode,Type,Message) :- name:guidance_errcode(ErrCode,Type,Message). % Let the main file handle anything we can't figure out
