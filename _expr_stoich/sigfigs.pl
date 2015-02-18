:- set_prolog_flag(double_quotes,chars).

sigfigs(H,T) --> leading_zeros(_), whole(H), decimal_part(H,T).

decimal_part([],T) -->  decimal_point, decimal_zeros(_), decimal(T).
decimal_part(_,[H|T]) --> decimal_point, decimal_zeros(H), decimal(T).
decimal_part(_,[]) --> "".


leading_zeros(["0"|T]) --> "0", leading_zeros(T).
leading_zeros([]) --> "".

number("0") --> "0".
number("1") --> "1".
number("2") --> "2".
number("3") --> "3".
number("4") --> "4".
number("5") --> "5".
number("6") --> "6".
number("7") --> "7".
number("8") --> "8".
number("9") --> "9".

whole([H|T]) --> number(H), whole(T).
whole([]) --> "".

decimal_point --> ".".

decimal_zeros(DZ) --> leading_zeros(DZ).

decimal(X) --> whole(X).
