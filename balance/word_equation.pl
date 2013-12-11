
word(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAWL,RAWR]) -->
	word_expression(Coeff,CoeffRest0,Elems,ElemRest0,Formula,FormulaRest0,RAWL),
	" --> ",
	{writeln('Right')},
	word_expression(CoeffRest0,CoeffRest,ElemRest0,ElemRest,FormulaRest0,FormulaRest,RAWR).


word_expression(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAWH|RAWT]) --> 
		word_balanced_formula(Coeff,CoeffRest0,Elems,ElemRest0,Formula,FormulaRest0,RAWH), 
		" + ", 
		{writeln('Add')},
		word_expression(CoeffRest0,CoeffRest,ElemRest0,ElemRest,FormulaRest0,FormulaRest,RAWT),
		!. 

word_expression(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAW]) --> 
	word_balanced_formula(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,RAW).

word_balanced_formula([Coeff|CoeffRest],CoeffRest,Elems,ElemRest,[Formula|FormulaRest],FormulaRest,Formula) --> 
	word_coefficient(Coeff),
	name(Elems,ElemRest,Formula).

word_coefficient(X) --> {nonvar(X), X = 1}, "". 
word_coefficient(2) --> "2 ".
word_coefficient(3) --> "3 ".
word_coefficient(4) --> "4 ".
word_coefficient(5) --> "5 ".
word_coefficient(6) --> "6 ".
word_coefficient(7) --> "7 ".
word_coefficient(8) --> "8 ".
word_coefficient(9) --> "9 ".
word_coefficient(10) --> "10 ".
word_coefficient(11) --> "11 ".
word_coefficient(12) --> "12 ".
word_coefficient(_) --> "". 

