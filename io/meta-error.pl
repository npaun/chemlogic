:- [error].
:- op(990,yfx,(xx)).
:- meta_predicate xx(//,?,?,?).

try --> "rrr" xx (no_woof,rodents).



Condition xx (SyntaxError,Flags) -->
	(Condition, !; syntax_stop(SyntaxError,Flags)).


Condition xx SyntaxError --> Condition xx (SyntaxError,[]).

