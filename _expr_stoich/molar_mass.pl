:- consult('../formula/compounder').


v_molar(22.414).

%%% Mass units %%%%
unit(Formula,Mass,g,Mol,mol) :-
	molar_mass(Formula,MMass),
	Mol /* mol */ is Mass /* g */ * 1 /* mol */ / MMass /* g */.

unit(Formula,Mol,mol,Mass,g) :-
	molar_mass(Formula,MMass),
	Mass /* g */ is Mol /* mol */ * MMass /* g */ / 1 /* mol */.

%%% Volume units %%%%
/*** NOTE:
Formula is assumed to represent a gas at STP (standard temperature and pressure)
***/


unit(_,Vol,'L',Mol,mol) :-
	v_molar(Vm),
	Vol /* L */ is Mol /* mol */ * Vm /* L */ / 1 /* mol */.

unit(_,Mol,mol,Vol,'L') :-
	v_molar(Vm),
	Mol /* mol */ is Vol /* L */ * 1 /* mol */ / Vm /* L */.



%%% Calculate the Molar Mass of a compound %%%
add_sum(Element,Sub,MassIn,MassOut) :-
	mass(Element,AtomicMass),!,
	MassOut is MassIn + AtomicMass * Sub.

	
sum_part([],_,X,X) :- !.
sum_part([[Sym,Sub]|PairS],GroupSub,Mass,MassF) :-
	add_sum(Sym, Sub * GroupSub,Mass,Mass1),
	sum_part(PairS,GroupSub,Mass1,MassF),
	!.

sum_part(Sym,Sub,Mass,MassF) :-
	add_sum(Sym, Sub, Mass, MassF).

molar_mass(Formula,MMass) :- molar_mass(Formula,0,MMass).

molar_mass([],X,X).

molar_mass([[Sym,Sub]|PairS],Mass,MassF) :-
	sum_part(Sym,Sub,Mass,Mass1),
	molar_mass(PairS,Mass1,MassF).
