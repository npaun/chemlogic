:- set_prolog_flag(double_quotes,chars).

sigfigs(Sig,SigR) --> leading_zeros(_,_), whole(Sig,SigR0), decimal_part(Sig,SigR0,SigR).

decimal_part([],Sig,SigR) -->  decimal_point, decimal_zeros(_,_), decimal(Sig,SigR).
decimal_part(_,Sig,SigR) --> decimal_point, decimal_zeros(Sig,SigR0), decimal(SigR0,SigR).
decimal_part(_,Sig,Sig) --> "".


leading_zeros(Sig,SigR) --> zero_part(Sig,SigR0), leading_zeros(SigR0,SigR).
leading_zeros(Sig,Sig) --> "".

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

zero_part(["0",SigR],SigR) --> "0".
num_part([Sig|SigR],SigR) --> number(Sig).

whole([Sig|SigR],SigR) --> num_part(Sig,SigR0), whole(SigR0,SigR).
whole(Sig,Sig) --> "".

decimal_point --> ".".

decimal_zeros(DZ,DE) --> leading_zeros(DZ,DE).

decimal(X,Y) --> whole(X,Y).
