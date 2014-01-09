:- include('oxyanion.pl').

% Inefficiencies to eliminate %
	% Oxyacids require a stupid test to determine the oxyanion
	% Stop the madness when testing multivalent metals
		% Does the issue come from ionic main or from something else?


% The algo in reverse:
/*	
	Pb1O2
	Pb2O1
	Pb4O1

Pb2O4
Pb 2 O 1

Al6N3
chromium(VI) nitride
Cr3N6
CrN2




*/


ionic(E,R,L) --> {var(L)}, ionic_fwd(E,R,L), {!}.
ionic(_,_,L) --> {nonvar(L)}, ionic_rev(L).

ionic_rev([[MSym,MSub],[NMSym,NMSub]]) -->
	{
	%  TODO: If the metal is monovalent, don't bother conjuring it up! Just use it.
	%  Also, no need to actually check these charges, is there?

(	charge(NMSym,NMCharge) ; oxyanion(_,_,NMSym,NMCharge,_,[]), !),
	!,
	NMTotal is abs(NMSub * NMCharge),
	MCharge is NMTotal / MSub
	},
	ionic_calcdata(_,_,[MSym,MCharge,NMSym,NMCharge]).

ionic_fwd(MElems,FinalRest,[[MSym,MSub],[NMSym,NMSub]]) --> ionic_calcdata(MElems,FinalRest,[MSym,MCharge,NMSym,NMCharge]),
	{
	GCD is gcd(MCharge,NMCharge),
	MSub is abs(NMCharge / GCD),
	NMSub is abs(MCharge / GCD)
	}.

ionic_calcdata(Contents,Rest,F) --> acid(Contents,Rest,F).
ionic_calcdata(MElems,FinalRest,[MSym,MCharge,NMSym,NMCharge]) --> cation(MElems,MRest,MSym,MCharge), " ", anion(MRest,FinalRest,NMSym,NMCharge).



metal(S,C) --> metal_multivalent(S,C).
metal(S,C) --> metal_monovalent(S,C).

metal_multivalent(Sym,Charge) -->  element(Sym,_),{charge(Sym,Charges), is_list(Charges)
	},
	multivalent_charge(Charge), {member(Charge,Charges)}.

metal_monovalent(Sym,Charge) --> element(Sym,_), {charge_check(metal,Sym,Charge)}.


cation(Elems,Rest,Formula,Charge) --> group(Elems,Rest,Formula,_),
	{
	charge_check(metal,Formula,Charge)
	}.


cation([S|Rest],Rest,S,C) --> metal(S,C).

anion(Elems,Rest,Formula,Charge) --> group(Elems,Rest,Formula,_),
	{
	charge_check(nonmetal,Formula,Charge)
	}.
anion(Contents,Rest,Formula,Charge) --> oxyanion(Contents,Rest,Formula,Charge).

anion([Sym|Rest],Rest,Sym,Charge) --> non_metal_ide(Sym,_),
	{
	charge(Sym,Charge)
	}.

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

acid(["H"|Contents],Rest,["H",1,ASym,ACharge]) --> acid_anion(Contents,Rest,ASym,ACharge), " acid".

acid_anion(Contents,Rest,ASym,ACharge) --> oxyanion_acid(Contents,Rest,ASym,ACharge).
acid_anion(Contents,Rest,ASym,ACharge) --> hydro_acid(Contents,Rest,ASym,ACharge).
acid_anion(Contents,Rest,ASym,ACharge) --> polyatomic_oxy_acid(Contents,Rest,ASym,ACharge).
acid_anion(Contents,Rest,ASym,ACharge) --> polyatomic_hydro_acid(Contents,Rest,ASym,ACharge).

hydro_acid([ASym|Rest],Rest,ASym,ACharge) --> "hydro", acid_base(ASym), "ic", {charge_check(nonmetal,ASym,ACharge)}.

acid_base("S") --> "sulfur".
acid_base("P") --> "phosphor".

acid_base(Sym) --> element_base(Sym,_).

polyatomic_oxy_acid(Contents,Rest,ASym,ACharge) --> group_base(Contents,Rest,ASym,Base), "ic", {\+ Base = "", member(["O",_],ASym), !, charge_check(nonmetal,ASym,ACharge)}.

/*
polyatomic_hydro_acid fails to prevent hydroacetic acid.

Also, perhaps the performance might be better if we put in group base, to avoid wasting our time recognizing "hydro".
*/

polyatomic_hydro_acid(Contents,Rest,ASym,ACharge) --> "hydro", group_base(Contents,Rest,ASym,Base), "ic", {\+ Base = "", charge_check(nonmetal,ASym,ACharge)}.
