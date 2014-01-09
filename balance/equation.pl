
symbolic(Mode,Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAWL,RAWR]) -->
	expression(Mode,Coeff,CoeffRest0,Elems,ElemRest0,Formula,FormulaRest0,RAWL),	
	output(Mode,arrow),
%	{writeln('Right')},
	expression(Mode,CoeffRest0,CoeffRest,ElemRest0,ElemRest,FormulaRest0,FormulaRest,RAWR).


expression(Mode,Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAWH|RAWT]) -->
		balanced_formula(Mode,Coeff,CoeffRest0,Elems,ElemRest0,Formula,FormulaRest0,RAWH),
		" + ",
%		{writeln('Add')},
		expression(Mode,CoeffRest0,CoeffRest,ElemRest0,ElemRest,FormulaRest0,FormulaRest,RAWT), !.




 expression(Mode,Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAW]) -->
	balanced_formula(Mode,Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,RAW).

balanced_formula(Mode,[Coeff|CoeffRest],CoeffRest,Elems,ElemRest,[Formula|FormulaRest],FormulaRest,Formula) -->
	coefficient(Coeff),
%	{writeln('Process')},
	formula(Mode,Elems,ElemRest,Formula), !.

coefficient(X) --> {nonvar(X), X = 1}, "".
coefficient(X) --> num_decimal(X).
coefficient(_) --> "".
