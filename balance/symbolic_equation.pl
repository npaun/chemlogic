:- module(symbolic,[symbolic//8]).
:- set_prolog_flag(double_quotes,chars).

symbolic(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideLeft,SideRight]) -->
	expr(Fmt,Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideLeft),
	output(Fmt,arrow) xx arrow,
	expr(Fmt,CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideRight).


expr(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,[SideH|SideT]) -->
	balanced_formula(Fmt,Coeff,CoeffR0,Elems,ElemR0,Formula,FormulaR0,SideH),
	expr_tail(Fmt,CoeffR0,CoeffR,ElemR0,ElemR,FormulaR0,FormulaR,SideT), !.

expr_tail(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side) -->
	" + ",
	expr(Fmt,Coeff,CoeffR,Elems,ElemR,Formula,FormulaR,Side).

expr_tail(_,Coeff,Coeff,Elems,Elems,Formula,Formula,[]) --> [].


balanced_formula(Fmt,[Coeff|CoeffR],CoeffR,Elems,ElemR,[Formula|FormulaR],FormulaR,Formula) -->
	coefficient(Coeff),
	formula(Fmt,Elems,ElemR,Formula,[]), !.

coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X).
coefficient(_) --> "".


:- include('symbolic_equation.msg').


% vi: ft=prolog
