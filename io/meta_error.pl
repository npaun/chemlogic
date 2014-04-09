% meta_error.pl: Some extremely simple meta-programming constructs that make error handling simpler
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun


:- consult(error).
:- op(990,yfx,(xx)).
:- op(990,yfx,(handle)).
:- meta_predicate xx(//,?,?,?).



Condition xx (SyntaxError,Flags) -->
	{Condition = Module:_},
	(Condition, !; syntax_stop(Module:SyntaxError,Flags)).


Condition xx SyntaxError --> Condition xx (SyntaxError,[]).


Clause handle HandlerArgs :-
	catch(
		Clause,
		error(InfoStruct,_),
		call(error_handler,HandlerArgs,InfoStruct)
	).

