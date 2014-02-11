:- set_prolog_flag(double_quotes,chars).


formula(_,[],[],[],[],[]) :- !.

formula(Fmt,Elems,ElemsR,[[Sym,Num]|FormulaR]) --> mol_species(Fmt,Elems,ElemsR0,Sym,Num), formula(Fmt,ElemsR0,ElemsR,FormulaR), !.

formula(Fmt,Elems,ElemsR,[[Sym,Num]]) --> mol_species(Fmt,Elems,ElemsR,Sym,Num).


mol_species(Fmt,Elems,ElemsR,Sym,Num) --> "(",poly_part(Fmt,Elems,ElemsR,Sym), ")", subscript(Fmt,Num),
	{
	Num > 1
	}.

mol_species(Fmt,Elems,ElemsR,Sym,Num) --> any_part(Fmt,Elems,ElemsR,Sym), subscript(Fmt,Num).


poly_part(Fmt,Elems,ElemsR,Formula) --> oxyanion_symbol(Fmt,Elems,ElemsR,Formula).

poly_part(user,Elems,ElemsR,Formula) --> group_symbol(Elems,ElemsR,Formula).
poly_part(output,Elems,ElemsR,Formula) --> group_symbol_output(Elems,ElemsR,Formula).

any_part(Fmt,Elems,ElemsR,Formula) --> poly_part(Fmt,Elems,ElemsR,Formula), !.
any_part(_,[Sym|ElemsR],ElemsR,Sym) --> element_symbol(Sym).

 oxyanion_symbol(Fmt,[Sym,"O"|ElemsR],ElemsR,[[Sym,1],["O",Num]]) -->  element_symbol(Sym), "O", subscript(Fmt,Num),
	{oxyanion([Sym,"O"],[],[[Sym,1],["O",Num]],_,_,[]), !}.



subscript(_,X) --> { \+ var(X), X = 1 }, [].
subscript(Fmt,X) --> output(Fmt,sub_start), num_decimal(X), output(Fmt,sub_end).
subscript(_,1) --> [].

