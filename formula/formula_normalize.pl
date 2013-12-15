:- set_prolog_flag(double_quotes,chars).

formula([],[],[],[],[]) :- !.

formula(Elems,Rest,[[Sym1,Num1]|Res]) --> (mol_species(Elems,Rest0,Sym1,Num1), formula(Rest0,Rest,Res), !); {
	var(Elems) ->
	syntax_error('Formula. Verify that the formula is correct. This error could also be raised by an element missing from the database.');
	throw('Logic Error: Formula. An internal structure could not be converted into a human readable formula.')}.

formula(Elems,Rest,[[Sym,Num]]) --> mol_species(Elems,Rest,Sym,Num).


mol_species(Elems,Rest,Sym,Num) --> "(",poly_part(Elems,Rest,Sym), ")", subscript(Num),
	{
	 Num > 1
	}.

mol_species(Elems,Rest,Sym,Num) --> any_part(Elems,Rest,Sym), subscript(Num).

/*
formula_symbol_TMP([Sym,"O"],[Sym,1,"O",Num]) --> element_symbol(Sym), "O", num_decimal(Num).
formula_symbol_TMP(Sym,Sym) --> element_symbol(Sym).
*/


poly_part(Elems,Rest,Formula) --> oxyanion_symbol(Elems,Rest,Formula).
poly_part(Elems,Rest,Formula) --> group_symbol(Elems,Rest,Formula).

any_part(Elems,Rest,Formula) --> poly_part(Elems,Rest,Formula), !.
any_part([Sym|Rest],Rest,Sym) --> element_symbol(Sym).

 oxyanion_symbol([Sym,"O"|Rest],Rest,[[Sym,1],["O",Num]]) -->  element_symbol(Sym), "O", subscript(Num), 
	{oxyanion([Sym,"O"],[],[[Sym,1],["O",Num]],_,_,[]), !}.



subscript(X) --> { \+ var(X), X = 1 }, [].
subscript(X) --> num_decimal(X).
subscript(1) --> [].

/*
num_decimal(X) --> { \+ var(X), X = 1 }, [].

num_decimal(10) --> "10".

num_decimal(2) --> "2".
num_decimal(3) --> "3".
num_decimal(4) --> "4".
num_decimal(5) --> "5".
num_decimal(6) --> "6".
num_decimal(7) --> "7".
num_decimal(8) --> "8".
num_decimal(9) --> "9".


num_decimal(1) --> [].
*/
