:- [error].
:- op(990,yfx,(xx)).
:- meta_predicate xx(//,?,?,?).



Condition xx (SyntaxError,Flags) -->
	{Condition = Module:_},
	(Condition, !; syntax_stop(Module:SyntaxError,Flags)).


Condition xx SyntaxError --> Condition xx (SyntaxError,[]).

