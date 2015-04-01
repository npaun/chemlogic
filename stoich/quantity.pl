% quantity.pl: Parses user-entered text quantity and query expressions.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(quantity,[quantity//1,query//1]).
:- use_module(sigfigs_number).
:- set_prolog_flag(double_quotes,chars).

quantity(qty([[Val,Unit]|Tail])) --> value(Val), " ", unit_sym(Unit), !, unit_tail(Unit,Tail), !.

value(value(Val,SF)) --> 
	({var(Val)} -> 
		(
			value_calc(value(Val,SF))
		);
		(
			value_display(value(Val,SF))
		)
	).

value_calc(value(Val,SF)) --> number(SFDigits,[],Digits,[]), {length(SFDigits,SF), number_chars(Val,Digits)}.
value_display(value(Val,_),H,T) :- format(chars(H,T),'~w',Val). % When performing output, it is much faster to just use SWI-Prolog's built-in conversion.

query(query([Unit|Tail],Property)) --> unit_sym(Unit), !, property(Property), unit_tail(Unit,Tail), !.

property(property(actual)) --> " reacted".
property(property(actual)) --> " produced".

property(property(excess)) --> " excess".

property(property(actual)) --> "".

unit_sym(unit(g)) --> "g".
unit_sym(unit('M')) --> "mol/L".
unit_sym(unit(mol)) --> "mol".
unit_sym(unit('L')) --> "L".
unit_sym(unit('M')) --> "M".

unit_tail(unit('L'),[[Val,unit('M')]]) --> " of ",  value(Val), " " ,unit_sym(unit('M')).
unit_tail(unit('L'),[[Val,unit('M')]]) --> " (",  value(Val), " " ,unit_sym(unit('M')), ")".
unit_tail(unit('M'),[[Val,unit('L')]]) --> " (",  value(Val), " " ,unit_sym(unit('L')), ")".

unit_tail(unit('M'),[[Val,unit(mol)]]) --> " (",  value(Val), " " ,unit_sym(unit(mol)), ")".
unit_tail(unit(mol),[[Val,unit('M')]]) --> " (",  value(Val), " " ,unit_sym(unit('M')), ")".

unit_tail(unit('L'),[[Val,unit(mol)]]) --> " (",  value(Val), " " ,unit_sym(unit(mol)), ")".
unit_tail(unit(mol),[[Val,unit('L')]]) --> " (",  value(Val), " " ,unit_sym(unit('L')), ")".

unit_tail(_,[]) --> [].
