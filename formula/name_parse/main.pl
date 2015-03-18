% main.pl: Common predicates and an interface for chemical naming rules
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(name,[name//3,nonmetal//3,nonmetal_ide//3,charge_check/2,charge_check/3]).
:- set_prolog_flag(double_quotes,chars). 
:- use_module(ionic).
:- use_module(covalent).
:- multifile guidance_errcode/3.



%%% Common testing subroutines %%%

nonmetal(Sym,Name,Charge) --> element(Sym,Name), {charge_check(nonmetal,Sym,Charge)}.

nonmetal_ide(Sym,Base,Charge) --> 
	element_base(Sym,Base), 
	{charge_check(nonmetal,Sym,Charge)} xx nonmetal_charge,
	"ide" xx ide.

	
/** charge_check(+Type,+Sym,?Charge) is semidet.
 ** charge_check(+Type,+Sym) is semidet.

Tests whether a given symbol is a nonmetal or metal. This is used in parsers to ensure that the user supplies a two nonmetals in the systematic_covalent parser or that a compound consists of a metal and a nonmetal in the ionic parser, for example.

It can also return the charge, because we need it to do the test anyway.

@vague	Type
@arg	Type	The type of element/polyatomic group Sym is supposed to be.	(nonmetal or metal).
@arg	Sym	An element/polyatomic groups symbol/internal formula.	Ag
@arg	Charge	An ionic charge or list of charges for an element or polyatomic ion	-1, [2,3], etc.
@todo	Include a test that verifies that Charge is really a Charge of a polyatomic group.

 */

charge_check(nonmetal,Sym,Charge) :-
        charge(Sym,Charge),
	\+ is_list(Charge),
        Charge =< 0.

charge_check(metal,Sym,Charge) :-
        charge(Sym,Charge),
	!,
        (is_list(Charge); Charge > 0),
        !.

charge_check(Type,Sym) :- charge_check(Type,Sym,_).


%%% Pure substances %%%



pure(Sym,Rest,Formula,Name,[]) :- pure_process(Sym,Rest,Formula,Name,[]).

/* REALLY UGLY HACK! */
pure(Sym,Rest,Formula)--> pure_process(Sym,Rest,Formula), pure_lookahead.
pure_lookahead, [L1], [L2] --> [L1], [L2],  {char_type(L2,punct)}.




pure_process(Sym,Rest,Formula) --> diatomic(Sym,Rest,Formula).
pure_process(Sym,Rest,Formula) --> single_element(Sym,Rest,Formula).

diatomic([Sym|Rest],Rest,[[Sym,2]]) --> element(Sym,_), {diatomic(Sym)}.
single_element([Sym|Rest],Rest,[[Sym,1]]) --> element(Sym,_).


%%% Combined parser %%%

name(Sym,Rest,Formula) --> retained(Sym,Rest,Formula).
name(Sym,Rest,Formula) --> {var(Formula)}, pure(Sym,Rest,Formula).
name(Sym,Rest,Formula) --> ionic(Sym,Rest,Formula).
name(Sym,Rest,Formula) --> covalent(Sym,Rest,Formula).
name(Sym,Rest,Formula) --> pure_process(Sym,Rest,Formula).
name(Sym,Rest,Formula) --> common(Sym,Rest,Formula).



%%%%% ERROR GUIDANCE - GENERAL %%%%%



guidance_unparsed([],
	'The program has processed the entire chemical name you have entered but has not found a required component.
	 Please check that you are not missing anything.

 	 The first missing component is '
 ).



%%%%% ERROR GUIDANCE FOR COMMON ROUTINES %%%%%



guidance_errcode(nonmetal,alpha,
	'The highlighted component is not a valid non-metal ide.
	 NOTE: It is possible that the non-metal is missing from the database.'
 ).

guidance_errcode(nonmetal,nil,
	'You are missing a non-metal in your compound.
	 Ionic compounds consist of: a positive ion (e.g. a metal) and a negative ion (e.g. a non-metal with an -ide ending).
	 e.g. sodium <chloride>

	 Covalent compounds consist of: a non-metal and a non-metal with an -ide ending.
	 e.g hydrogen mon<oxide>'
 ).

guidance_errcode(nonmetal,white,Message) :- guidance_errcode(nonmetal,nil,Message).


guidance_errcode(nonmetal,_,
	'You have entered the highlighted incorrect characters instead of a valid non-metal with an -ide ending.'
).

guidance_errcode(nonmetal_charge,alpha,
	'You have entered a metal, but it does not make sense here. 
	 Ionic compounds are formed from a metal and a non-metal (or a cation and anion).
 	 Covalent compounds are formed from two non-metals.'
 ).

guidance_errcode(ide,alpha,
	'The non-metal you have entered must end in "ide". This error is easy to fix: just change the junk you have entered (highlighted) to ide!


	 Ionic compounds consist of: a positive ion (e.g. a metal) a non-metal with an -ide ending.
	 NOTE: If the compound you are entering is ionic, you may have misspelled the name of a negative ion instead.

	 Covalent compounds consist of: a non-metal and a non-metal with an -ide ending.
	 e.g hydrogen mon<oxide>'
 ).

guidance_errcode(ide,nil,
	'The non-metal you have enterd must end in "ide". This error is easy to fix: just type ide at the end of the compound!'
).



%%%%% ERROR GUIDANCE COMMON TO ALL NAME PARSERS %%%%%



guidance_errcode(fail,alpha,
	'The chemical name you are entering (starting with the highlighted component) does not conform to any known naming rules.
	 Please check that you are correctly following one of these rules:

	 1. Ionic: positive ion (e.g. a metal) negative ion (e.g. a non-metal with an -ide ending)
	 e.g sodium chloride

	 2. Acid: hydro (if it does not contain oxygen) non-metal suffix acid
	 e.g. hydrosulfuric acid

	 3. Covalent: non-metal non-metalide
	 e.g. dihydrogen monoxide

	 4. Allotropes of nonmetals: prefixnon-metal
	 e.g. trioxygen

	 5. Some hydrocarbons: prefix ane/ene/anol
	 e.g. methane

	 6. Retained names (preferred) and some common names:
	 e.g. water, baking soda


	 NOTE: The program does not support any other naming conventions, yet. Sorry.

 	 If you can get your input to follow one of these conventions, as briefly described, 
	 the program will be able to give you more detailed help.
 ').

guidance_errcode(fail,nil,
	'If you do not enter anything, what are we supposed to do, here?'
).

guidance_errcode(fail,_,
 	'Remove any incorrect or unnecessary characters from highlighted component.'
).

guidance_errcode(none,white,
	'The program only supports the following simple compound types:

	 1. Ionic: positive ion (e.g. a metal) negative ion (e.g. a non-metal with an -ide ending)
	 e.g sodium chloride

	 2. Acid: hydro (if it does not contain oxygen) non-metal suffix acid
	 e.g. hydrosulfuric acid

	 3. Covalent: non-metal non-metalide
	 e.g. dihydrogen monoxide

	 4. Allotropes of nonmetals: prefixnon-metal
	 e.g. trioxygen

	 5. Some hydrocarbons: prefix ane/ene/anol
	 e.g. methane

	 6. Retained names (preferred) and some common names:
	 e.g. water, baking soda'
 ).

 guidance_errcode(none,alpha,
 	'Your input would be a valid chemical compound name if it was not for the highlighted characters on the end. 
	 If they are unnecessary, please remove them.
 	 
 	 Otherwise, you may have to correct your compound name, or the program may not support the naming convention you are using.'
 ).



%%%%% CATCH-ALL GUIDANCE %%%%%



guidance_errcode(_,nil,''). %Let the unparsed guidance deal with this
guidance_errcode(_,white,
	'You have inserted a space where it does not make sense. 

	1. You forgot to type whatever component goes after the space.
	Please insert it.

	2. The space is unecessary and all of the required components are present.
	Please remove it.'
).

guidance_errcode(_,_,
	'You have probably entered some incorrect characters. Remove them or correct them.'
).
