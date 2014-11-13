% This file was obtained from http://people.sju.edu/~jhodgson/wg17/Drafts/DCGs/reference_implementation
% This is the reference implementation of the draft ISO standard specification for Prolog definite clause grammars.
% ISO/IEC JTC1/SC22/WG17
% It is maintained by Dr. Jonathan Hodgson, St. Joseph's University.


/**********************************************************************************

11 Reference implementations

The reference implementations provided in this section do not preclude alternative 
or optimized implementations.

11.1 Grammar-rule translator

This section provides a reference implementation for a translator of grammar-rules 
into Prolog clauses as specified in the ISO/IEC 13211{1 Prolog standard. The main 
idea is to translate grammar rules into clauses by adding two extra arguments to 
each grammar rule non-terminal, following the logical expansion of grammar rules, 
described in the previous section.  The  first extra argument  is  used for  the 
comprehensive terminal-sequence.        The second extra argument is used for the 
remaining terminal-sequence. This is a straight-forward solution. Nevertheless, 
compliance with this TR does not imply this specific translation solution, only 
compliance with the logical expansion, as specified in section 10.
This translator includes error-checking code that ensures that both the input 
grammar rule and the resulting clause are valid. In addition, this translator 
attempts to simplify the resulting clauses by removing redundant calls to true/0 
and by folding unifications. In some cases, the resulting clauses could be further 
optimized. Other optimizations can be easily plugged in, by modifying or extending 
the dcg simplify/4 predicate. However, implementers must be careful to delay output 
unifications in the presence of goals with side-effects such as cuts or input/output 
operations, ensuring the steadfastness of the generated  clauses.

***********************************************************************************/

% converts a grammar rule into a normal clause:

dcg_rule(Rule, Clause) :-
	dcg_rule(Rule, S0, S, Expansion),
	dcg_simplify(Expansion, S0, S, Clause).

dcg_rule((RHead --> _), _, _, _) :-
	var(RHead),
	throw(instantiation_error).

dcg_rule((RHead, _ --> _), _, _, _) :-
	var(RHead),
	throw(instantiation_error).

dcg_rule((_, Terminals --> _), _, _, _) :-
	var(Terminals),
	throw(instantiation_error).

dcg_rule((NonTerminal,Terminals --> GRBody), S0, S, (Head :- Body)):-
	!,
	dcg_non_terminal(NonTerminal, S0, S, Head),
	dcg_body(GRBody, S0, S1, Goal1),
	dcg_terminals(Terminals, S, S1, Goal2),
	Body = (Goal1, Goal2).


dcg_rule((NonTerminal --> GRBody), S0, S, (Head :- Body)) :-
	!,
	dcg_non_terminal(NonTerminal, S0, S, Head),
	dcg_body(GRBody, S0, S, Body).

dcg_rule(Term, _, _, _) :-
	throw(type_error(grammar_rule, Term)).
 % translates a grammar goal non-terminal:

dcg_non_terminal(NonTerminal, _, _, _) :-
	\+ callable(NonTerminal),
	throw(type_error(callable, NonTerminal)).

dcg_non_terminal(NonTerminal, S0, S, Goal) :-
	NonTerminal =.. NonTerminalUniv,
	append(NonTerminalUniv, [S0, S], GoalUniv),
	Goal =.. GoalUniv.

% translates a terminal-sequence:

dcg_terminals(Terminals, _, _, _) :-
	\+ is_proper_list(Terminals),
	throw(type_error(list, Terminals)).

dcg_terminals(Terminals, S0, S, S0 = List) :-
	append(Terminals, S, List).

% translates a grammar rule body:

dcg_body(Var, S0, S, phrase(Var, S0, S)) :-
	var(Var),
	!.

dcg_body((GRIf -> GRThen), S0, S, (If -> Then)) :-
	!,
	dcg_body(GRIf, S0, S1, If),
	dcg_body(GRThen, S1, S, Then).

dcg_body((GREither; GROr), S0, S, (Either; Or)) :-
	!,
	dcg_body(GREither, S0, S, Either),
	dcg_body(GROr, S0, S, Or).

dcg_body((GRFirst, GRSecond), S0, S, (First, Second)) :-
	!,
	dcg_body(GRFirst, S0, S1, First),
	dcg_body(GRSecond, S1, S, Second).

dcg_body(!, S0, S, (!, S0 = S)) :-
	!.

dcg_body({}, S0, S, (S0 = S)) :-
	!.

dcg_body({Goal}, S0, S, (call(Goal), S0 = S)) :-
	var(Goal),
	!.

dcg_body({Goal}, _, _, _) :-
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

dcg_body({Goal}, S0, S, (Goal, S0 = S)) :-
	!.

dcg_body(\+ GRBody, S0, S, (\+ Goal, S0 = S)) :-
	!,
	dcg_body(GRBody, S0, S, Goal).

dcg_body([], S0, S, (S0=S)) :-
	!.

dcg_body([T| Ts], S0, S, Goal) :-
	!,
	dcg_terminals([T| Ts], S0, S, Goal).

dcg_body(NonTerminal, S0, S, Goal) :-
	dcg_non_terminal(NonTerminal, S0, S, Goal).

% simplifies the resulting clause:

dcg_simplify((Head :- Body), _, _, Clause) :-
	dcg_conjunctions(Body, Flatted),
	dcg_fold_left(Flatted, FoldedLeft),
	dcg_fold_pairs(FoldedLeft, FoldedPairs),
	( FoldedPairs == true ->
	Clause = Head
	; 	Clause = (Head :- FoldedPairs)
	).

% removes redundant calls to true/0 and flattens conjunction of goals:

dcg_conjunctions((Goal1 -> Goal2), (SGoal1 -> SGoal2)) :-
	!,
	dcg_conjunctions(Goal1, SGoal1),
	dcg_conjunctions(Goal2, SGoal2).

dcg_conjunctions((Goal1; Goal2), (SGoal1; SGoal2)) :-
	!,
	dcg_conjunctions(Goal1, SGoal1),
	dcg_conjunctions(Goal2, SGoal2).

dcg_conjunctions(((Goal1, Goal2), Goal3), Body) :-
	!,
	dcg_conjunctions((Goal1, (Goal2, Goal3)), Body).

dcg_conjunctions((true, Goal), Body) :-
	!,
	dcg_conjunctions(Goal, Body).

dcg_conjunctions((Goal, true), Body) :-
	!,
	dcg_conjunctions(Goal, Body).

dcg_conjunctions((Goal1, Goal2), (Goal1, Goal3)) :-
	!,
	dcg_conjunctions(Goal2, Goal3).

dcg_conjunctions(\+ Goal, \+ SGoal) :-
	!,
	dcg_conjunctions(Goal, SGoal).

dcg_conjunctions(Goal, Goal).

 % folds left unifications:

dcg_fold_left((Term1 = Term2), true) :-
	!,
	Term1 = Term2.

dcg_fold_left(((Term1 = Term2), Goal), Folded) :-
	!,
	Term1 = Term2,
	dcg_fold_left(Goal, Folded).

dcg_fold_left(Goal, Goal).

% folds pairs of consecutive unifications (T1 = T2, T2 = T3):

dcg_fold_pairs((Goal1 -> Goal2), (SGoal1 -> SGoal2)) :-
	!,
	dcg_fold_pairs(Goal1, SGoal1),
	dcg_fold_pairs(Goal2, SGoal2).

dcg_fold_pairs((Goal1; Goal2), (SGoal1; SGoal2)) :-
	!,
	dcg_fold_pairs(Goal1, SGoal1),
	dcg_fold_pairs(Goal2, SGoal2).

dcg_fold_pairs(((T1 = T2a), (T2b = T3)), (T1 = T3)) :-
	T2a == T2b,
	!.

dcg_fold_pairs(((T1 = T2a), (T2b = T3), Goal), ((T1 = T3), Goal2)) :-
	T2a == T2b,
	!,
	dcg_fold_pairs(Goal, Goal2).

dcg_fold_pairs((Goal1, Goal2), (Goal1, Goal3)) :-
	!,
	dcg_fold_pairs(Goal2, Goal3).

dcg_fold_pairs(\+ Goal, \+ SGoal) :-
	!,
	dcg_fold_pairs(Goal, SGoal).

dcg_fold_pairs(Goal, Goal).


/*************************************************************************

11.1.1 Extended version for Prolog compilers with encapsulation mechanisms

Assuming that the infix operator :/2 is used for calling predicates inside 
an encapsulation unit, the following clause would allow translation of 
grammar rule bodies that explicitly use non-terminals from another 
encapsulation unit:

*************************************************************************/

dcg_body(Unit:GRBody, S0, S, Unit:Goal) :-
	!,
	dcg_body(GRBody, S0, S, Goal).



% One possible problem with this clause is that any existence errors when 
%executing the goal Unit:Goal will most likely be expressed in terms of 
%the expandedpredicates and not in terms of the original grammar rule 
%non-terminals. In order to more easily report errors at the same 
%abstraction level as grammar rules, the following alternative clause 
%may be used:


dcg_body(Unit:GRBody, S0, S, Unit:phrase(GRBody, S0, S)) :-
	!,
	dcg_body(GRBody, S0, S, _). % ensure that GRBody is valid



/*************************************************************************

11.2 phrase/3

This section provides a reference implementation in Prolog of the built-in 
predicates phrase/3. It includes the necessary clauses for error handling, 
as specified  in section 8.1.1.3. For the reference implementation of 
phrase/2 see section  8.1.1.4.

**************************************************************************/

phrase(GRBody, S0, S) :-
	var(GRBody),
	throw(error(instantiation_error, phrase(GRBody, S0, S))).

phrase(GRBody, S0, S) :-
	\+ callable(GRBody),
	throw(error(type_error(callable, GRBody), phrase(GRBody, S0,
      S))).

phrase(GRBody, S0, S) :-
	nonvar(S0),
	\+ compound(S0),
	throw(error(type_error(list, S0), phrase(GRBody, S0, S))).

phrase(GRBody, S0, S) :-
	nonvar(S),
	\+ compound(S),
	throw(error(type_error(list, S), phrase(GRBody, S0, S))).

phrase(GRBody, S0, S) :-
	dcg_body(GRBody, TS0, TS, Goal),
	TS0 = S0, TS = S,
	call(Goal).


% The predicate dcg body/4 is part of the grammar rule translator reference 
% implementation, defined in section 11.1. An alternative, informal 
% implementation  of phrase/3 using a meta-interpreter is presented in 
% the Annex A.

/**************************************************************************

11.3 Auxiliary predicates used on the reference implementations

The following auxiliary predicates are used on the reference implementations.
The are commented out so far, because they are available in most implementations.

*/
append([], List, List).

append([Head| Tail], List, [Head| Tail2]) :-

append(Tail, List, Tail2).

/*callable(Term) :-
	nonvar(Term),
	functor(Term, Functor, _),
	atom(Functor).
	 is_list([]) :-
	!.
*/

is_list([_|Tail]) :-
	is_list(Tail).


is_proper_list(List) :-
	List == [], !.

is_proper_list([_| Tail]) :-
	nonvar(Tail),
	is_proper_list(Tail).

/***********************************************************************/

