:- [error].
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

