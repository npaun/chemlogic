:- module(oxyanion,[oxyanion//4,oxyanion_acid//4]).
:- set_prolog_flag(double_quotes,chars).
:- use_module(ionic,[acid_base//1]).



%%% Test to check if user-provided oxyanion really exists %%% 

exists(Sym,List,Oxygens,Charge) --> 
	(
		{oxyanions(Sym,Charge,List)}; 
		syntax_stop(no_oxyanions)
	), 

	(
		{Oxygens > 0}; 
		syntax_stop(invalid_oxyanion)
	).


%%% Oxyanion names -- for purposes of explanation; rather slow for other uses %%% 

oxyanion([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> oxyanion(Sym,Oxygens,Charge).


oxyanion(Sym,Oxygens,Charge) --> "per", element_base(Sym,_), "ate",
		exists(Sym,[Oxygens,_,_,_],Oxygens,Charge).

oxyanion(Sym,Oxygens,Charge) --> element_base(Sym,_), "ate",
		exists(Sym,[_,Oxygens,_,_],Oxygens,Charge).

oxyanion(Sym,Oxygens,Charge) --> element_base(Sym,_), "ite",
		exists(Sym,[_,_,Oxygens,_],Oxygens,Charge).

oxyanion_(Sym,Oxygens,Charge) --> "hypo", element_base(Sym,_), "ite",
		exists(Sym,[_,_,_,Oxygens],Oxygens,Charge).


%%% Oxyanion acid names -- for purposes of explanation; rather slow for other uses %%% 

oxyanion_acid([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> oxyanion_acid(Sym,Oxygens,Charge).


oxyanion_acid(Sym,Oxygens,Charge) --> "per",  acid_base(Sym), "ic",
		exists(Sym,[Oxygens,_,_,_],Oxygens,Charge).

oxyanion_acid(Sym,Oxygens,Charge) --> acid_base(Sym) , "ic",
		exists(Sym,[_,Oxygens,_,_],Oxygens,Charge).

oxyanion_acid(Sym,Oxygens,Charge) --> acid_base(Sym), "ous",
		exists(Sym,[_,_,Oxygens,_],Oxygens,Charge).

oxyanion_acid(Sym,Oxygens,Charge) --> "hypo", acid_base(Sym), "ous",
		exists(Sym,[_,_,_,Oxygens],Oxygens,Charge).

/* CORRECTOR; remove if unecessary */
oxyanion_acid(Sym,_,_) --> 
	("per"; "hypo"; {true}), 
	acid_base(Sym), 
	(
		(
			("ate"; "ite"), 
			syntax_stop(corrector_oxyanion_acid)
		);

		(
			" acid",
			syntax_stop(corrector_oxyanion_missing)
		)
	).



%%%%% ERROR CODE GUIDANCE %%%%%



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
	 
	 1. Oxyacids (probably what you were intending):

	 All ions ending in -ate (including per--ates) are changed to ic:
	 e.g perchlor<ate> becomes perchlor<ic>

	 All ions ending in -ite (including hypo--ites) are changed to ous:
	 e.g. hypochlor<ite> becomes hypochlor<ous>
	 
	 NOTE: Check that your prefixes correspond with your suffixes!

	 2. Acids not containing oxygen:

	 You also forgot the hydro- prefix, in that case.
 	 The acid will end in -ic.
	 e.g. <hydro>sulfur<ic> acid
	 '
 ).
