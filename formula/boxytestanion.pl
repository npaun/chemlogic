make_oxy :-
	oxyanions(Elem,Charge,[PerAte,Ate,Ite,HypoIte]),
	writeln(Elem),
	PerAte > 0 -> writeln('PerAte'),
	Ate > 0 -> writeln('Ate').

	fix_formula(Elem,Charge,Ate) :-
		assertz(charge([[Elem,1],["O",Ate]],Charge)).


per_mate(Elem,Charge,PerAte) :- writeln((Elem,Charge,PerAte)).
mate(Elem,Charge,Ate) :- writeln((Elem,Charge,Ate)).

mite(Elem,Charge,Ite) :- writeln((Elem,Charge,Ite)).
hypo_mite(Elem,Charge,HypoIte) :- writeln((Elem,Charge,HypoIte)).
