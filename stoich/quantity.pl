% quantity.pl: Parses user-entered text quantity and query expressions.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(quantity,[quantity//1,quantity_prefix//1,query//1,query_result//1,queries_convert/2]).
:- use_module(sigfigs).
:- use_module(sigfigs_number).
:- set_prolog_flag(double_quotes,chars).


quantity_prefix(Qty) --> quantity(Qty), " ".
quantity_prefix(nil) --> "".

quantity([[Val,Unit]|Tail]) --> value(Val) xx not_a_number, " ", unit_sym(Unit) xx invalid_unit, !, unit_tail(Unit,Tail), !.

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
value_display([Val,SF],H,T) :- round_sigfigs(Val,SF,ValRound), format(chars(H,T),'~w',ValRound). % When performing output, it is much faster to just use SWI-Prolog's built-in conversion.

queries_convert([],[]) :- !.

queries_convert([Var-Input|InputS],[Output|OutputS]) :-
	atom_chars(Input,InputChars),
	query(Output,Var-InputChars,[]),
	queries_convert(InputS,OutputS), !.

queries_convert([Input|InputS],[Output|OutputS]) :-
	query(Output,Input,[]),
	queries_convert(InputS,OutputS).

query(nil,nil,[]).
query(Struct,Var-Input,Rest) :- query(Struct,Var,Input,Rest).
query([[[[Var,_],Unit]|Tail],Property],Var) --> unit_sym(Unit), !, property(Property), unit_tail(Unit,Tail), !.

query_result(Struct) --> {Struct = [[[[Val,SF],_]|_],_]}, value_display([Val,SF]), " ", query(Struct,Val).

property(excess) --> " excess".
property(Type,Input,Rest) :- (var(Input) -> Input = Rest; property_label(Type,Input,Rest)).

property_label(actual) --> " reacted".
property_label(actual) --> " produced".

property_label(excess) --> " excess".


property_label(actual) --> "".

unit_sym(g) --> "g".
unit_sym('M') --> "M".
unit_sym('M') --> "mol/L".
unit_sym(mol) --> "mol".
unit_sym('L') --> "L".

unit_tail('L',[[Val,'M']]) --> " of ",  value(Val), " " ,unit_sym('M').
unit_tail('L',[[Val,'M']]) --> " (",  value(Val), " " ,unit_sym('M'), ")".
unit_tail('M',[[Val,'L']]) --> " (",  value(Val), " " ,unit_sym('L'), ")".

unit_tail('M',[[Val,mol]]) --> " (",  value(Val), " " ,unit_sym(mol), ")".
unit_tail(mol,[[Val,'M']]) --> " (",  value(Val), " " ,unit_sym('M'), ")".

unit_tail('L',[[Val,mol]]) --> " (",  value(Val), " " ,unit_sym(mol), ")".
unit_tail(mol,[[Val,'L']]) --> " (",  value(Val), " " ,unit_sym('L'), ")".

unit_tail(_,[]) --> [].

%%%% GUIDANCE FOR ERRORS %%%%%

guidance_unparsed(_,
	'The program has processed the entire chemical quantity you have entered, but it is missing a required component.

	 The first missing component is: '
	 ).

guidance_errcode(fail,_,
	'The chemical quantity you have entered cannot be recognized.
	 Please check to ensure that you have entered it correctly.

	 Chemical quantities consist of a value followed by a unit.
	 e.g. 5 g, 33004.30 mol

	 A quantity can also be described using a value and a unit, followed by another value and a unit, in parentheses.
	 e.g. 5 L (9 M)'
	 ).

guidance_errcode(invalid_unit,_,
	'The unit you have entered is invalid.
	 This may be due to either because you have mistyped the name of a unit, a valid unit is not supported by the program, or the unit you have entered is inappropriate for this type of quantity.

	 Please check to ensure that you have used the correct unit for the given quantity.'
	 ).

guidance_errcode(not_a_number,_,
	'You have entered an invalid character where a number was expected.
	Please verify that you have entered the chemical equation and all quantities correctly.'
	).
