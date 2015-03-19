% sigfigs_number.pl: The DCG for identifying the significant digits of a number.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun

:- set_prolog_flag(double_quotes,chars).


number(Digit,DigitR) --> leading_zeros(_,_), % Leading zeros in the whole part are not significant. 
	(
		(
			sequence_not_null(Digit,DigitR0) -> decimal_part(DigitR0,DigitR), !; % If the whole number component is not zero, then leading zeros in the decimal part of the number are significant. 
			decimal_part_nozero(Digit,DigitR), ! % Otherwise, they are just placeholders
		);
		zero_result(Digit,DigitR) % If absolutely no significant digits have been identified, then the number must be 0.
	).

zero_result([0],[]) --> "".


decimal_part_nozero(Digit,DigitR) --> decimal_point, leading_zeros(Zeros,ZerosR), sequence_tail(Sequence,SequenceR), 
	(
		{Sequence = []} -> {Digit = Zeros, ZerosR = [0|DigitR]};  % If the decimal part consists entirely of zeros, then they are significant. A zero must also have been present in the whole part.
		{Digit = Sequence, DigitR = SequenceR} % Otherwise, throw away leading zeros, because they are only placeholders.
	).

% The decimal part of a number can consist of actual digits, a decimal point only to clarify precision, or nothing (i.e. it is not present).
decimal_part(Digit,DigitR) --> decimal_point, leading_zeros(Digit,DigitR0), sequence_tail(DigitR0,DigitR).
decimal_part(Digit,Digit) --> decimal_point.
decimal_part(Digit,Digit) --> "".

% Identify any leading zeros in a sequence.
leading_zeros([0|DigitR0],DigitR) --> "0", leading_zeros(DigitR0,DigitR), !.
leading_zeros(Digit,Digit) --> "".

% A sequence of digits that contains at least one digit.
sequence_not_null([Digit|DigitR0],DigitR) --> digit(Digit), sequence_tail(DigitR0,DigitR).

% A sequence of digits that may be null.
sequence_tail([Digit|DigitR0],DigitR) --> digit(Digit), sequence_tail(DigitR0,DigitR), !.
sequence_tail(Digit,Digit) --> "",!.

digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".
digit(6) --> "6".
digit(7) --> "7".
digit(8) --> "8".
digit(9) --> "9".

decimal_point --> ".".

