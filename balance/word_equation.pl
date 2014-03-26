:- module(word_equation,[word//8]).
:- set_prolog_flag(double_quotes,chars).

word(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideLeft,SideRight]) -->
	expr(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideLeft),
	output(Fmt,arrow) xx word_equation:arrow,
	expr(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideRight).


expr(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideH|SideT]) --> 
	balanced_name(Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideH),
	expr_tail(CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideT), !.

expr_tail(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[Side]) --> 
	" + ",
	expr(Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side).

expr_tail(Coeff,Coeff,Elems,Elems,Formula,Formula,[]) --> [].


balanced_name([Coeff|CoeffR],CoeffR,Elems,ElemR,[Formula|FormulaR],FormulaR,Formula) -->
	coefficient(Coeff),
	name(Elems,ElemR,Formula),
	!.

coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X), " ".
coefficient(_) --> "".

% vi: ft=prolog
