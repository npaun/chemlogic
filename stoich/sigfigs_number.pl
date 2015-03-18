number(Digit,DigitR) --> leading_zeros(_,_), % Leading zeros in the whole part are not significant. 
	(
		(
			nonzero_sequence(Digit,DigitR0) -> decimal_part(DigitR0,DigitR), !; % If the whole number component is not zero, then leading zeros in the decimal part of the number are significant. 
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

% A sequence of digits that does not start with (and consist entirely of) zeros.
nonzero_sequence([Digit|DigitR0],DigitR) --> digit(Digit),!, sequence_tail(DigitR0,DigitR).

% Zeros may be contained in the sequence.
sequence_tail([Digit|DigitR0],DigitR) --> any_digit(Digit), sequence_tail(DigitR0,DigitR), !.
sequence_tail(Digit,Digit) --> "",!.

any_digit(Digit) --> digit(Digit).
any_digit(0) --> "0".

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

