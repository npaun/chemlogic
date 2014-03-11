formula(Fmt,Elems,ElemsR,Formula,FormulaR) --> formula_part_first(Fmt,Elems,ElemsR0,Formula,FormulaR0), (hydrate_part(Fmt,ElemsR0,ElemsR,FormulaR0,FormulaR), !).

hydrate_part(Fmt,["H","O"|ElemsR],ElemsR,[["H2O",Coeff]|SymR],SymR) --> " . ", number(Coeff), water_output(Fmt).
hydrate_part(_,Elems,Elems,Part,Part) --> [].

formula_part_first(Fmt,Elems,ElemsR,Part,PartR) --> part(Fmt,Type,Elems,ElemsR0,Part,PartR0), (formula_part(Fmt,Type,ElemsR0,ElemsR,PartR0,PartR), !).

formula_part(Fmt,multi,Elems,ElemsR,Part,PartR) --> part(Fmt,_,Elems,ElemsR0,Part,PartR0), (formula_part(Fmt,_,ElemsR0,ElemsR,PartR0,PartR), !).
formula_part(_,none,Elems,Elems,Part,Part) --> [].

%part(Elems,ElemsR,Part,R) --> poly_part(Elems,ElemsR,Part,R).
%part(Elems,ElemsR,Part,R) --> simple_part(Elems,ElemsR,Part,R).

part(Fmt,multi,Elems,ElemsR,[[Sym,Num]|PartR],PartR) --> "(",group(Elems,ElemsR,Sym),")",number(Num).
part(Fmt,multi,Elems,ElemsR,[[Sym,1]|PartR],PartR) --> group(Elems,ElemsR,Sym).
part(Fmt,_,[Elem|ElemsR],ElemsR,[[Elem,Num]|PartR],PartR) --> element(Elem), (subscript(Fmt,Num), !).


subscript(Fmt,X) --> number(X).
subscript(_,1) --> [].

number(2) --> "2".
number(3) --> "3".
number(4) --> "4".

element("C") --> "C".
element("H") --> "H".
element("O") --> "O".
element("N") --> "N".
element("S") --> "S".

group(["N","H"|ElemsR],ElemsR,[["N",1],["H",4]]) --> "NH4".
group(["S","O"|ElemsR],ElemsR,[["S",1],["O",3]]) --> "SO3".
