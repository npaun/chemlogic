:- dynamic(balance/3).

/*
IMPROVEMENTS:

Use call to fix RHS?
Use a completely different function for RHS?
*/


% A polyatomic group, consisting of multiple pairs (N1H4 = N1, H4).

balance_store(Group,Key,Value) :-
	(retract(balance(Group,Key,OldValue)) -> NewValue is OldValue + Value; NewValue = Value),
	assertz(balance(Group,Key,NewValue)).



store_subpart(_,[],_,_).

store_subpart(Original,[[Sym,Sub]|PairS],PolySub,ExprSign) :-
%	SignedSub is Sub * PolySub * ExprSign,
	balance_store(Original,Sym,Sub * PolySub * ExprSign),
	store_subpart(Original,PairS,PolySub,ExprSign).

% Just a pair (H2 or C4, etc, etc.)

store_subpart(Original,Sym,Sub,1) :-
	balance_store(Original,Sym,Sub).


store_subpart(Original,Sym,Sub,-1) :-
	balance_store(Original,Sym,-Sub).


rebase_formula(_,[],_) :- !.


rebase_formula(Original,[[Sym,Sub]|T],Sign) :-
	store_subpart(Original,Sym,Sub,Sign),
	!,
	rebase_formula(Original,T,Sign).

rebase_expression([],_) :- !.

rebase_expression([H|T],Sign) :-
	rebase_formula(H,H,Sign),
	rebase_expression(T,Sign).

rebase([L,R]) :-
	rebase_expression(L,1),
	rebase_expression(R,-1).

