
word(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideLeft,SideRight]) -->
	word_expression(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideLeft),
	output(Fmt,arrow),
	word_expression(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideRight).


word_expression(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideH|SideT]) -->
	word_balanced_formula(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideH),
	" + ",
	word_expression(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideT),
	!.

word_expression(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[Side]) -->
	word_balanced_formula(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side).

word_balanced_formula([Coeff|CoeffR],CoeffR,Elems,ElemR,[Formula|FormulaR],FormulaR,Formula) -->
	word_coefficient(Coeff),
	name(Elems,ElemR,Formula),
	!.

word_coefficient(X) --> {nonvar(X), X = 1}, "".
word_coefficient(X) --> num_decimal(X), " ".
word_coefficient(_) --> "".

% vi: syntax=prolog
