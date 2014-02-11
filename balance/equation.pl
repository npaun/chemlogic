
symbolic(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideLeft,SideRight]) -->
	expression(Fmt,Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideLeft),
	output(Fmt,arrow),
	expression(Fmt,CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideRight).


expression(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideH|SideT]) -->
	balanced_formula(Fmt,Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideH),
	" + ",
	expression(Fmt,CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideT), !.

expression(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[Side]) -->
	balanced_formula(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side).

balanced_formula(Fmt,[Coeff|CoeffR],CoeffR,Elems,ElemR,[Formula|FormulaR],FormulaR,Formula) -->
	coefficient(Coeff),
	formula(Fmt,Elems,ElemR,Formula), !.

coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X).
coefficient(_) --> "".

% vi: syntax=prolog
