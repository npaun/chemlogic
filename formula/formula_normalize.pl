:- set_prolog_flag(double_quotes,chars).


formula([],[],[],[],[]) :- !.

formula(Elems,Rest,[[Sym1,Num1]|Res]) --> mol_species(Elems,Rest0,Sym1,Num1), formula(Rest0,Rest,Res), !.

formula(Elems,Rest,[[Sym,Num]]) --> mol_species(Elems,Rest,Sym,Num).


mol_species(Elems,Rest,Sym,Num) --> "(",poly_part(Elems,Rest,Sym), ")", subscript(Num),
	{
	 Num > 1
	}.

mol_species(Elems,Rest,Sym,Num) --> any_part(Elems,Rest,Sym), subscript(Num).


poly_part(Elems,Rest,Formula) --> oxyanion_symbol(Elems,Rest,Formula).
poly_part(Elems,Rest,Formula) --> group_symbol(Elems,Rest,Formula).

any_part(Elems,Rest,Formula) --> poly_part(Elems,Rest,Formula), !.
any_part([Sym|Rest],Rest,Sym) --> element_symbol(Sym).

 oxyanion_symbol([Sym,"O"|Rest],Rest,[[Sym,1],["O",Num]]) -->  element_symbol(Sym), "O", subscript(Num), 
	{oxyanion([Sym,"O"],[],[[Sym,1],["O",Num]],_,_,[]), !}.



subscript(X) --> { \+ var(X), X = 1 }, [].
subscript(X) --> num_decimal(X).
subscript(1) --> [].

