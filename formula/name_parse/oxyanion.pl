% oxyanion.pl: A rather inefficient on-the-fly DCG for oxyanion names --- used to help catch syntax errors
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(oxyanion,[oxyanion//4,oxyanion_acid//4]).
%:- set_prolog_flag(double_quotes,chars).
:- use_module(acid,[acid_base//1]).



%%% Test to check if user-provided oxyanion really exists %%% 

exists(Sym,Charge,List,Oxygens) --> 
	{oxyanions(Sym,Charge,List)} xx no_oxyanions,
	{Oxygens > 0} xx invalid_oxyanion. 


find(Sym,Charge,Oxygens,N) --> 
	{oxyanions(Sym,Charge,List)} xx no_oxyanions, 
	{nth0(N,List,Oxygens)} xx invalid_oxyanion.


%%% Oxyanion names -- for purposes of explanation; rather slow for other uses %%% 

oxyanion([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> {nonvar(Charge)}, !, 
	find(Sym,Charge,Oxygens,N), 
	oxyanion_(Sym,Charge,Oxygens,N).

oxyanion([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> oxyanion_(Sym,Charge,Oxygens,_).



oxyanion_(Sym,Charge,Oxygens,0) --> "per", element_base(Sym,_), "ate",
		exists(Sym,Charge,[Oxygens,_,_,_],Oxygens).

oxyanion_(Sym,Charge,Oxygens,1) --> element_base(Sym,_), "ate",
		exists(Sym,Charge,[_,Oxygens,_,_],Oxygens).

oxyanion_(Sym,Charge,Oxygens,2) --> element_base(Sym,_), "ite",
		exists(Sym,Charge,[_,_,Oxygens,_],Oxygens).

oxyanion_(Sym,Charge,Oxygens,3) --> "hypo", element_base(Sym,_), "ite",
		exists(Sym,Charge,[_,_,_,Oxygens],Oxygens).


%%% Oxyanion acid names -- for purposes of explanation; rather slow for other uses %%% 

oxyanion_acid([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> {nonvar(Charge)}, !,
	find(Sym,Charge,Oxygens,N), 
	oxyanion_acid_(Sym,Charge,Oxygens,N).

oxyanion_acid([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> oxyanion_acid_(Sym,Charge,Oxygens,_).


oxyanion_acid_(Sym,Charge,Oxygens,0) --> "per",  acid_base(Sym), "ic",
		exists(Sym,Charge,[Oxygens,_,_,_],Oxygens).

oxyanion_acid_(Sym,Charge,Oxygens,1) --> acid_base(Sym) , "ic",
		exists(Sym,Charge,[_,Oxygens,_,_],Oxygens).

oxyanion_acid_(Sym,Charge,Oxygens,2) --> acid_base(Sym), "ous",
		exists(Sym,Charge,[_,_,Oxygens,_],Oxygens).

oxyanion_acid_(Sym,Charge,Oxygens,3) --> "hypo", acid_base(Sym), "ous",
		exists(Sym,Charge,[_,_,_,Oxygens],Oxygens).

/* CORRECTOR; remove if unecessary */
oxyanion_acid_(Sym,_,_,_) --> 
	("per"; "hypo"; {true}), 
	acid_base(Sym), 
	(
		(
			("ate"; "ite"), 
			syntax_stop(oxyanion:corrector_oxyanion_acid)
		);

		(
			" acid",
			syntax_stop(oxyanion:corrector_oxyanion_missing,mark)
		)
	).



%%%%% ERROR MESSAGE GUIDANCE %%%%%



guidance_unparsed([],
	'The program has parsed your entire chemical name and detected an oxyanion, but has not found a required component.
	 Please check that there is nothing missing from the name of your oxyacid.
	
 	 The first thing that is missing is: '
 ).
 	 
guidance_errcode(invalid_oxyanion,_,
	'The oxyanion you have entered does not exist (or it is not in the database).
	 Please ensure that you are not inventing a new oxyanion.

 	e.g There is no persulfate, hyponitrite, etc.'
).

guidance_errcode(no_oxyanions,_,
	'The element you have entered does not have any oxyanions (of the supported type).
	 Please ensure that you are not inventing a new oxyanion
	 

	 e.g There is no xenate or hypocuprite'
 ).

guidance_errcode(corrector_oxyanion_acid,_,
	'BUSTED! When forming acids, oxyanion suffixes are changed.

	 All ions ending in -ate (including per--ates) are changed to ic:
	 e.g perchlor<ate> becomes perchlor<ic>

	 All ions ending in -ite (including hypo--ites) are changed to ous:
	 e.g. hypochlor<ite> becomes hypochlor<ous>
	 
	 NOTE: Check that your prefixes correspond with your suffixes!').

guidance_errcode(corrector_oxyanion_missing,_,
	'BUSTED! All acids have a suffix depending on their type. You forgot to enter it.
	 Please correct the acid left of the highlighted mark.
	 
	 1. Oxyacids (probably what you were intending):

	 All ions ending in -ate (including per--ates) are changed to ic:
	 e.g perchlor<ate> becomes perchlor<ic>

	 All ions ending in -ite (including hypo--ites) are changed to ous:
	 e.g. hypochlor<ite> becomes hypochlor<ous>
	 
	 NOTE: Check that your prefixes correspond with your suffixes!

	 2. Acids not containing oxygen:

	 You also forgot the hydro- prefix, in that case.
 	 The acid will end in -ic.
	 e.g. <hydro>sulfur<ic> acid'
 ).


guidance_errcode(ErrCode,Type,Message) :- name:guidance_errcode(ErrCode,Type,Message). % Let the main file handle anything we can't figure out
