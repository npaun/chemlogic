
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
coefficient(2) --> "2".
coefficient(3) --> "3".
coefficient(4) --> "4".
coefficient(5) --> "5".
coefficient(6) --> "6".
coefficient(7) --> "7".
coefficient(8) --> "8".
coefficient(9) --> "9".
coefficient(10) --> "10".
coefficient(11) --> "11".
coefficient(12) --> "12".
coefficient(13) --> "13".
coefficient(14) --> "14".

coefficient(_) --> "".
