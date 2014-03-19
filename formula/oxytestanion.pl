
oxy_to_dcg(_,_,[],[],[]).

oxy_to_dcg(Elem,Charge,[Oxygen|OxygenS],[Prefix|PrefixS],[Suffix|SuffixS]) :-
	(Oxygen > 0 -> 
		(
			oxy_formula(Elem,Charge,Oxygen,Formula),
			oxy_name(Prefix,Elem,Suffix,Name,Base),
			cl_poly_to_dcg(auto(Formula,Name,Base))
		);
		true),
	oxy_to_dcg(Elem,Charge,OxygenS,PrefixS,SuffixS).

oxy_formula(Elem,Charge,Oxygens,Formula) :-
	Formula = [[Elem,1],["O",Oxygens]],
	writeln(Formula),
	assertz(charge(Formula,Charge)).

oxy_name(Prefix,Elem,Suffix,Name,Base) :-
	cl(element(Elem,_,ElemBase)),
	append(Prefix,ElemBase,Base),
	append(Base,Suffix,Name).

% vi: ft=prolog
