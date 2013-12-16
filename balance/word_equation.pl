
word(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAWL,RAWR]) -->
	word_expression(Coeff,CoeffRest0,Elems,ElemRest0,Formula,FormulaRest0,RAWL),
	" --> ",
%	{writeln('Right')},
	word_expression(CoeffRest0,CoeffRest,ElemRest0,ElemRest,FormulaRest0,FormulaRest,RAWR).


word_expression(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAWH|RAWT]) --> 
		word_balanced_formula(Coeff,CoeffRest0,Elems,ElemRest0,Formula,FormulaRest0,RAWH), 
		" + ", 
%		{writeln('Add')},
		word_expression(CoeffRest0,CoeffRest,ElemRest0,ElemRest,FormulaRest0,FormulaRest,RAWT),
		!. 

word_expression(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,[RAW]) --> 
	word_balanced_formula(Coeff,CoeffRest,Elems,ElemRest,Formula,FormulaRest,RAW).

word_balanced_formula([Coeff|CoeffRest],CoeffRest,Elems,ElemRest,[Formula|FormulaRest],FormulaRest,Formula) --> 
	word_coefficient(Coeff),
	name(Elems,ElemRest,Formula),
	!.
%	{writeln(Formula)}.

word_coefficient(X) --> {nonvar(X), X = 1}, "". 
word_coefficient(X) --> num_decimal(X), " ".
word_coefficient(_) --> "". 

