% sigfigs_number.pl: The DCG for identifying the significant digits of a number.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun

:- module(sigfigs_number,[number//4]).
:- set_prolog_flag(double_quotes,chars).


number(Sig,SigR,Digit,DigitR) --> leading_zeros(_,_,Digit,DigitR0), % Leading zeros in the whole part are not significant. 
	(
		(
			sequence_not_null(Sig,SigR0,DigitR0,DigitR1) -> (decimal_part(SigR0,SigR,DigitR1,DigitR), !); % If the whole number component is not zero, then leading zeros in the decimal part of the number are significant. 
			decimal_part_nozero(Sig,SigR,DigitR0,DigitR)  % Otherwise, they are just placeholders
		), !;
		zero_result(Sig,SigR,Digit,DigitR) % If absolutely no significant digits have been identified, then the number must be 0.
	).

zero_result(['0'],[],['0'],[]) --> "".


decimal_part_nozero(Sig,SigR,Digit,DigitR) --> decimal_point(Digit,DigitR0), leading_zeros(Zeros,ZerosR,DigitR0,DigitR1), sequence_tail(Sequence,SequenceR,DigitR1,DigitR), 
	(
		{Sequence = []} -> {Sig = Zeros, ZerosR = [0|SigR]};  % If the decimal part consists entirely of zeros, then they are significant. A zero must also have been present in the whole part.
		{Sig = Sequence, SigR = SequenceR} % Otherwise, throw away leading zeros, because they are only placeholders.
	).

% The decimal part of a number can consist of actual digits, a decimal point only to clarify precision, or nothing (i.e. it is not present).
decimal_part(Sig,SigR,Digit,DigitR) --> decimal_point(Digit,DigitR0), leading_zeros(Sig,SigR0,DigitR0,DigitR1), sequence_tail(SigR0,SigR,DigitR1,DigitR).
decimal_part(Sig,Sig,Digit,Digit) --> decimal_point(_,_).
decimal_part(Sig,Sig,Digit,Digit) --> "".

% Identify any leading zeros in a sequence.
leading_zeros(['0'|SigR0],SigR,['0'|DigitR0],DigitR) --> "0", leading_zeros(SigR0,SigR,DigitR0,DigitR).
leading_zeros(Sig,Sig,Digit,Digit) --> "".

% A sequence of digits that contains at least one digit.
sequence_not_null([Sig|SigR0],SigR,[Sig|DigitR0],DigitR) --> digit(Sig), sequence_tail(SigR0,SigR,DigitR0,DigitR).

% A sequence of digits that may be null.
sequence_tail([Sig|SigR0],SigR,[Sig|DigitR0],DigitR) --> digit(Sig), sequence_tail(SigR0,SigR,DigitR0,DigitR).
sequence_tail(Sig,Sig,Digit,Digit) --> "".

digit('0') --> "0".
digit('1') --> "1".
digit('2') --> "2".
digit('3') --> "3".
digit('4') --> "4".
digit('5') --> "5".
digit('6') --> "6".
digit('7') --> "7".
digit('8') --> "8".
digit('9') --> "9".

decimal_point(['.'|R],R) --> ".".

