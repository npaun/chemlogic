% sigfigs.pl: Implements the calculation and rounding of numbers using signficant figures.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(sigfigs,[sigfigs/2,round_sigfigs/3, to_number/2]).
:- consult(sigfigs_number).
:- set_prolog_flag(double_quotes,chars).


/*** TODO:
 	* Implement scientific notation.
	* Make the treatment of numbers with trailing zeros in the whole part configurable.
***/

round_precision(0,_,0). % The rounding logic will not work if the input is 0. The untruncator will produce the correct precision.
round_precision(Number,Precision,Result) :-
	WholeLength is ceil(log10(abs(Number))), % Calculate the number of digits in the whole number part.
	Power is Precision - WholeLength,
	Magnitude is 10 ** Power,
	Shifted is round(Number * Magnitude), % Shift the number so that the built-in rounding logic will round the decimal part.
	Rounded is Shifted / Magnitude, % Shift the number back to its correct magnitude.
	(Rounded =:= integer(Rounded) -> Result is integer(Rounded); Result = Rounded). % Sometimes Prolog will return an integer as a float with a spurious decimal place. This will remove it.


round_untruncate(Rounded,Precision,Fixed) :-
	TotalLen is Precision + 1,
	atom_length(Rounded,Length),
	(TotalLen > Length ->

	((sub_atom(Rounded,_,1,_,'.') -> ( % If there already is a decimal point, we only need to re-add zeros that were truncated by Prolog.
					FmtString = '~w~`0t~'
				);
				( % Otherwise, the decimal point must be added as well.
					FmtString = '~w.~`0t~'
				)
	),
	atomic_list_concat([FmtString,TotalLen,'+'],'',Format),
	format(atom(Fixed),Format,Rounded)
	); Rounded = Fixed).
				

round_sigfigs(Value,SF,Result) :-
	round_precision(Value,SF,Rounded),
	round_untruncate(Rounded,SF,Result).


to_number(Atom,Number) :-
	number_chars(Number,Atom).

sigfigs(Value,SF) :-
	number(SigDigits,[],Value,[]), % Identify all of the significant figures in the number.
	length(SigDigits,SF). % Count them to identify the number of signifcant figures.


