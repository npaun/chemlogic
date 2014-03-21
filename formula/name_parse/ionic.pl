:- [oxyanion].

% Inefficiencies to eliminate %
	% Oxyacids require a stupid test to determine the oxyanion
	% Stop the madness when testing multivalent metals
	% Does the issue come from ionic main or from something else?


ionic(Elems,ElemsR,Formula) --> {var(Formula)}, ionic_fwd(Elems,ElemsR,Formula), {!}.
ionic(_,_,Formula) --> {nonvar(Formula)}, ionic_rev(Formula).

ionic_rev([[MSym,MSub],[NMSym,NMSub]|Appended]) -->
	{
	%  TODO: If the metal is monovalent, don't bother conjuring it up! Just use it.
	%  Also, no need to actually check these charges, is there?


	(charge(NMSym,NMCharge), !),
	!,
	NMTotal is abs(NMSub * NMCharge),
	MCharge is NMTotal / MSub
	},
	ionic_calcdata(_,_,[MSym,MCharge,NMSym,NMCharge],Appended),
	!,
	/* Corrector: remove if unecessary */
	({GCD is gcd(MSub,NMSub)}, ({GCD = 1} -> {true}; syntax_stop(not_reduced))).

ionic_fwd(MElems,FinalRest,[[MSym,MSub],[NMSym,NMSub]|Appended]) --> ionic_calcdata(MElems,FinalRest,[MSym,MCharge,NMSym,NMCharge],Appended),
	{
	GCD is gcd(MCharge,NMCharge),
	MSub is abs(NMCharge / GCD),
	NMSub is abs(MCharge / GCD)
	}.

ionic_calcdata(Elems,Rest,Formula,[]) --> acid(Elems,Rest,Formula).
ionic_calcdata(MElems,FinalRest,[MSym,MCharge,NMSym,NMCharge],Hydrate) --> 
	cation(MElems,MRest,MSym,MCharge), 
	" ", 
	anion(MRest,NMRest,NMSym,NMCharge), 
	optional_hydrate(NMRest,FinalRest,Hydrate).

optional_hydrate(["H","O"|ElemR],ElemR,[[[["H",2],["O",1]],Num]]) --> 
	" ", 
	((sub_general(Num,Suffix), Suffix); syntax_stop(hydrate_number)), 
	("hydrate"; syntax_stop(hydrate_h2o)).
optional_hydrate(Pass,Pass,[]) --> [].

metal(Sym,Charge) --> element(Sym,_), {charge(Sym,Charges)}, metal_valence(Sym,Charge,Charges).

metal_valence(Sym,Charge,Charges) --> 
	{is_list(Charges)},
       	(multivalent_charge(Charge); syntax_stop(charge)), 
	({member(Charge,Charges)}; syntax_stop(charge_invalid)).



	metal_valence(Sym,Charge,Charges) --> {Charges > 0},
       	({var(Charge)} -> multivalent_corrector; {true}), /* CORRECTOR: remove if unecessary */	
	{Charge = Charges}.

/* CORRECTOR: remove if unnecessary */
multivalent_corrector --> (multivalent_charge(_) -> syntax_stop(corrector_not_multivalent); {true}).

cation(Elems,Rest,Formula,Charge) --> group(Elems,Rest,Formula,_),
	{
	charge_check(metal,Formula,Charge)
	}.


cation([Sym|Rest],Rest,Sym,Charge) --> metal(Sym,Charge).

anion(Elems,Rest,Formula,Charge) --> group(Elems,Rest,Formula,_), ({charge_check(nonmetal,Formula,Charge)}; syntax_stop(anion)).

anion([Sym|Rest],Rest,Sym,Charge) --> (nonmetal_ide(Sym,_,Charge); syntax_stop(nonmetal)).




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

acid(["H"|Elems],Rest,["H",1,ASym,ACharge]) --> acid_anion(Elems,Rest,ASym,ACharge), (" acid"; syntax_stop(acid)).

acid_anion(Elems,Rest,ASym,ACharge) --> hydro_acid(Elems,Rest,ASym,ACharge).
acid_anion(Elems,Rest,ASym,ACharge) --> polyatomic_oxy_acid(Elems,Rest,ASym,ACharge).
acid_anion(Elems,Rest,ASym,ACharge) --> polyatomic_hydro_acid(Elems,Rest,ASym,ACharge).
acid_anion(Elems,Rest,ASym,ACharge) --> oxyanion_acid(Elems,Rest,ASym,ACharge).

hydro_acid([ASym|Rest],Rest,ASym,ACharge) --> "hydro", acid_base(ASym), ic_suffix,
	({charge_check(nonmetal,ASym,ACharge)}; syntax_stop(nonmetal_acid)).

	%
	%
	%
	%oxyanion_acid(Elems,Rest,ASym,ACharge) --> group_base(Elems,Rest,ASym,_), 
	%{ASym = [[ElemSym,_],_]}, (acid_ion_suffix(ElemSym), !; syntax_stop(acid_ion_suffix)), 
	%{charge(ASym,ACharge)}, 
	%(acid_oxyanion_suffix(ASym) /*xxx*/).

polyatomic_oxy_acid(Elems,Rest,ASym,ACharge) --> group_base(Elems,Rest,ASym,Base), ic_suffix, 
(	{	member(["O",_],ASym), !} ; ({var(Elems)} -> syntax_stop(oxy_acid_rule))), 
	({charge_check(nonmetal,ASym,ACharge)}; syntax_stop(nonmetal_acid)).

/*
Also, perhaps the performance might be better if we put in group base, to avoid wasting our time recognizing "hydro".
*/

polyatomic_hydro_acid(Elems,Rest,ASym,ACharge) --> "hydro", group_base(Elems,Rest,ASym,Base), ic_suffix, 
 (	{ \+ member(["O",_],ASym), ! }; syntax_stop(oxy_acid_rule)), 
		({charge_check(nonmetal,ASym,ACharge)}; syntax_stop(nonnmetal_acid)).


ic_suffix --> ("ic" -> {true}; syntax_stop(ic_acid_suffix)).
