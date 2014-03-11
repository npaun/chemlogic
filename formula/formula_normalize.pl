:- set_prolog_flag(double_quotes,chars).

formula(Fmt,Elems,ElemsR,Formula,FormulaR) -->
	part(Fmt,Type,Elems,ElemsR0,Formula,FormulaR0),
	formula_tail_special(Fmt,Type,ElemsR0,ElemsR1,FormulaR0,FormulaR1),
	formula_final(Fmt,ElemsR1,ElemsR,FormulaR1,FormulaR).

formula_tail_special(Fmt,one_or_more,Elems,ElemsR,Formula,FormulaR) -->
	part(Fmt,_,Elems,ElemsR0,Formula,FormulaR0),
	formula_tail(Fmt,ElemsR0,ElemsR,FormulaR0,FormulaR).
formula_tail_special(_,none,Elems,Elems,Formula,Formula) --> [].



formula_tail(_,[],[],[],[],[],[]).

formula_tail(Fmt,Elems,ElemsR,Formula,FormulaR) -->
	part(Fmt,_,Elems,ElemsR0,Formula,FormulaR0),
	formula_tail(Fmt,ElemsR0,ElemsR,FormulaR0,FormulaR).


formula_tail(_,Elems,Elems,Formula,Formula) --> [].

formula_final(Fmt,Elems,ElemsR,[Formula|FormulaR],FormulaR) --> hydrate_part(Fmt,Elems,ElemsR,Formula).
formula_final(_,Elems,Elems,Formula,Formula) --> [].

hydrate_part(Fmt,Elems,ElemsR,[Sym,Num]) --> " . ", num_decimal(Num), water_symbol(Fmt,Elems,ElemsR,Sym).

water_symbol(Fmt,["H","O"|ElemsR],ElemsR,[["H",2],["O",1]]) --> "H", subscript(Fmt,2), "O".

part(_,_,[],[],[],[],[],[]).

part(Fmt,one_or_more,Elems,ElemsR,[Sym|SymR],SymR) --> poly_part_1(Fmt,Elems,ElemsR,Sym).
part(Fmt,one_or_more,Elems,ElemsR,[Sym|SymR],SymR) --> poly_part_multi(Fmt,Elems,ElemsR,Sym).
part(Fmt,_,[Elem|ElemR],ElemR,[[Elem,Num]|PairR],PairR) --> element_symbol(Elem), subscript(Fmt,Num).


poly_part_1(Fmt,Elems,ElemsR,[Sym,1]) --> poly_symbol(Fmt,Elems,ElemsR,Sym).
poly_part_multi(Fmt,Elems,ElemsR,[Sym,Num]) --> "(", poly_symbol(Fmt,Elems,ElemsR,Sym), ")", num_decimal(Num).

/* Dumb Name */
poly_symbol(Fmt,Elems,ElemsR,Sym) --> group_symbol(Fmt,Elems,ElemsR,Sym).
 poly_symbol(Fmt,Elems,ElemsR,Sym) --> oxyanion_symbol(Fmt,Elems,ElemsR,Sym).

oxyanion_symbol(Fmt,[Sym,"O"|ElemsR],ElemsR,[[Sym,1],["O",Num]]) -->
	element_symbol(Sym),
	"O", subscript(Fmt,Num),
	{oxyanion([Sym,"O"],[],[[Sym,1],["O",Num]],_,_,[])}.


% Subscripts

% subscript(X,Y) --> rubscript(X,Y).

subscript(_,X) --> { nonvar(X) -> X = 1}, [].
subscript(Fmt,X) --> output(Fmt,sub_start), num_decimal(X), output(Fmt,sub_end).
subscript(_,1) --> [].

mubscript(_,X) --> num_decimal(X).
mubscript(_,1) --> [].
