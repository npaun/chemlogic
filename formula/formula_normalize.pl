:- set_prolog_flag(double_quotes,chars).


formula(_,[],[],[],[],[]) :- !.

formula(Mode,Elems,Rest,[[Sym1,Num1]|Res]) --> mol_species(Mode,Elems,Rest0,Sym1,Num1), formula(Mode,Rest0,Rest,Res), !.

formula(Mode,Elems,Rest,[[Sym,Num]]) --> mol_species(Mode,Elems,Rest,Sym,Num).


mol_species(Mode,Elems,Rest,Sym,Num) --> "(",poly_part(Mode,Elems,Rest,Sym), ")", subscript(Mode,Num),
	{
	 Num > 1
	}.

mol_species(Mode,Elems,Rest,Sym,Num) --> any_part(Mode,Elems,Rest,Sym), subscript(Mode,Num).


poly_part(Mode,Elems,Rest,Formula) --> oxyanion_symbol(Mode,Elems,Rest,Formula).
poly_part(_,Elems,Rest,Formula) --> group_symbol(Elems,Rest,Formula).

any_part(Mode,Elems,Rest,Formula) --> poly_part(Mode,Elems,Rest,Formula), !.
any_part(_,[Sym|Rest],Rest,Sym) --> element_symbol(Sym).

 oxyanion_symbol(Mode,[Sym,"O"|Rest],Rest,[[Sym,1],["O",Num]]) -->  element_symbol(Sym), "O", subscript(Mode,Num), 
	{oxyanion([Sym,"O"],[],[[Sym,1],["O",Num]],_,_,[]), !}.



subscript(_,X) --> { \+ var(X), X = 1 }, [].
subscript(Mode,X) --> output(Mode,sub_start), num_decimal(X), output(Mode,sub_end).
subscript(_,1) --> [].

