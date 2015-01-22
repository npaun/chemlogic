:- consult('../formula/compounder').

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
