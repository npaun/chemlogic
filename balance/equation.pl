
symbolic(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAWL,RAWR]) -->
	expression(Coeff,CoeffRest0,Elems,ElemRest0,Formula,FormulaRest0,RAWL),	
	" --> ",
%	{writeln('Right')},
	expression(CoeffRest0,CoeffRest,ElemRest0,ElemRest,FormulaRest0,FormulaRest,RAWR).


expression(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAWH|RAWT]) --> 
		balanced_formula(Coeff,CoeffRest0,Elems,ElemRest0,Formula,FormulaRest0,RAWH), 
		" + ", 
%		{writeln('Add')},
		expression(CoeffRest0,CoeffRest,ElemRest0,ElemRest,FormulaRest0,FormulaRest,RAWT), !. 




 expression(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAW]) --> 
	balanced_formula(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,RAW).

balanced_formula([Coeff|CoeffRest],CoeffRest,Elems,ElemRest,[Formula|FormulaRest],FormulaRest,Formula) --> 
	coefficient(Coeff), 
%	{writeln('Process')},
	formula(Elems,ElemRest,Formula), !.

coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X).
coefficient(_) --> "".
