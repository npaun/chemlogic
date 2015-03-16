:- set_prolog_flag(double_quotes,chars).

sigfigs(Digit,DigitR) --> leading_zeros(_,_), (new_digit(Digit,DigitR0) -> decimal_part(DigitR0,DigitR), !; decimal_part_nozero(Digit,DigitR), !).

decimal_part_nozero(Digit,DigitR) --> decimal_point, leading_zeros(_,_), new_digit(Digit,DigitR).

decimal_part(Digit,DigitR) --> decimal_point, leading_zeros(Digit,DigitR0), new_digit(DigitR0,DigitR).
decimal_part(Digit,Digit) --> decimal_point.
decimal_part(Digit,Digit) --> "".



leading_zeros([0|DigitR0],DigitR) --> "0", leading_zeros(DigitR0,DigitR).
leading_zeros(Digit,Digit) --> "".



new_digit([Digit|DigitR0],DigitR) --> digit(Digit),!, new_digit_tail(DigitR0,DigitR).
new_digit_tail([Digit|DigitR0],DigitR) --> really_digit(Digit), new_digit_tail(DigitR0,DigitR), !.
new_digit_tail(Digit,Digit) --> "",!.

really_digit(Digit) --> digit(Digit).
really_digit(Digit) --> zero(Digit).


zero(0) --> "0".
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
