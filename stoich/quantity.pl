:- consult(sigfigs_number).

quantity(qty(value(Val,SF),unit(Unit))) --> number(SFDigits,[]), {length(SFDigits,SF)}, " ", unit_sym(Unit).

unit_sym(g) --> "g".
unit_sym(mol) --> "mol".
unit_sym('L') --> "L".
unit_sym('M') --> "M".
unit_sym('M') --> "mol/L".

