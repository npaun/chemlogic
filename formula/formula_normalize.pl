formula(Fmt,Elems,ElemsR,Formula,FormulaR) --> formula_part_first(Fmt,Elems,ElemsR0,Formula,FormulaR0), (hydrate_part(Fmt,ElemsR0,ElemsR,FormulaR0,FormulaR), !).

hydrate_part(Fmt,["H","O"|ElemsR],ElemsR,[[Formula,Coeff]|SymR],SymR) --> output(Fmt,dot), num_decimal(Coeff), water_output(Fmt,Formula).
hydrate_part(_,Elems,Elems,Part,Part) --> [].

water_output(Fmt,[["H",2],["O",1]]) --> "H", output(Fmt,sub_start), "2", output(Fmt,sub_end),"O".

formula_part_first(Fmt,Elems,ElemsR,Part,PartR) --> part(Fmt,Type,Elems,ElemsR0,Part,PartR0), (formula_part(Fmt,Type,ElemsR0,ElemsR,PartR0,PartR), !).

formula_part(Fmt,multi,Elems,ElemsR,Part,PartR) --> part(Fmt,_,Elems,ElemsR0,Part,PartR0), (formula_part(Fmt,_,ElemsR0,ElemsR,PartR0,PartR), !).
formula_part(_,none,Elems,Elems,Part,Part) --> [].

%part(Elems,ElemsR,Part,R) --> poly_part(Elems,ElemsR,Part,R).
%part(Elems,ElemsR,Part,R) --> simple_part(Elems,ElemsR,Part,R).

part(Fmt,multi,Elems,ElemsR,[[Sym,Num]|PartR],PartR) --> "(",group_symbol(Fmt,Elems,ElemsR,Sym),")",num_decimal(Num).
part(Fmt,multi,Elems,ElemsR,[[Sym,1]|PartR],PartR) --> group_symbol(Fmt,Elems,ElemsR,Sym).
part(Fmt,_,[Elem|ElemsR],ElemsR,[[Elem,Num]|PartR],PartR) --> element_symbol(Elem), (subscript(Fmt,Num), !).

subscript(_,X) --> { nonvar(X) -> X = 1}, [].
subscript(Fmt,X) --> output(Fmt,sub_start),num_decimal(X),output(Fmt,sub_end).
subscript(_,1) --> [].


