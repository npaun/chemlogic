% quantity.pl: Parses user-entered text quantity and query expressions.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(quantity,[quantity//1,query//1]).
:- use_module(sigfigs_number).
:- set_prolog_flag(double_quotes,chars).

quantity([[Val,Unit]|Tail]) --> value(Val), " ", unit_sym(Unit), !, unit_tail(Unit,Tail), !.

value([Val,SF]) --> 
	({var(Val)} -> 
		(
			value_calc([Val,SF])
		);
		(
			value_display([Val,SF])
		)
	).

value_calc([Val,SF]) --> number(SFDigits,[],Digits,[]), {length(SFDigits,SF), number_chars(Val,Digits)}.
value_display([Val,_],H,T) :- format(chars(H,T),'~w',Val). % When performing output, it is much faster to just use SWI-Prolog's built-in conversion.

query([[Unit|Tail],Property]) --> unit_sym(Unit), !, property(Property), unit_tail(Unit,Tail), !.

property(actual) --> " reacted".
property(actual) --> " produced".

property(excess) --> " excess".

property(actual) --> "".

unit_sym(g) --> "g".
unit_sym('M') --> "mol/L".
unit_sym(mol) --> "mol".
unit_sym('L') --> "L".
unit_sym('M') --> "M".

unit_tail('L',[[Val,'M']]) --> " of ",  value(Val), " " ,unit_sym('M').
unit_tail('L',[[Val,'M']]) --> " (",  value(Val), " " ,unit_sym('M'), ")".
unit_tail('M',[[Val,'L']]) --> " (",  value(Val), " " ,unit_sym('L'), ")".

unit_tail('M',[[Val,mol]]) --> " (",  value(Val), " " ,unit_sym(mol), ")".
unit_tail(mol,[[Val,'M']]) --> " (",  value(Val), " " ,unit_sym('M'), ")".

unit_tail('L',[[Val,mol]]) --> " (",  value(Val), " " ,unit_sym(mol), ")".
unit_tail(mol,[[Val,'L']]) --> " (",  value(Val), " " ,unit_sym('L'), ")".

unit_tail(_,[]) --> [].
