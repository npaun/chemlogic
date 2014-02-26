:- module(word_equation,[word//8]).
:- set_prolog_flag(double_quotes,chars).

word(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideLeft,SideRight]) -->
	expression(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideLeft),
	output(Fmt,arrow),
	expression(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideRight).


expression(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideH|SideT]) -->
	balanced_formula(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideH),
	" + ",
	expression(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideT),
	!.

expression(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[Side]) -->
	balanced_formula(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side).

balanced_formula([Coeff|CoeffR],CoeffR,Elems,ElemR,[Formula|FormulaR],FormulaR,Formula) -->
	coefficient(Coeff),
	name(Elems,ElemR,Formula),
	!.

coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X), " ".
coefficient(_) --> "".

% vi: syntax=prolog
