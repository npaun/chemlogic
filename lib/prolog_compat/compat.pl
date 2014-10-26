
writeln(Atom) :- write(Atom), nl.


% Definition copied from SWI-Prolog (library/lists.pl)
% Copyright (C): 1985-2011, University of Amsterdam
% 			    VU University Amsterdam

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).



% Definition copied from SWI-Prolog (library/lists.pl)
% Copyright (C): 1985-2011, University of Amsterdam
% 			    VU University Amsterdam
%	@author Gertjan van Noord

% The SWI-Prolog builtin memberchk/2 is faster, but it is not available on some Prologs and its definition is not available to copy.

member(El, [H|T]) :-
    member_(T, El, H).

member_(_, El, El).
member_([H|T], El, _) :-
    member_(T, El, H).



% Recursive Euclidean algorithm
% Requires 5 inferences in most cases
% Does not evaluate arguments arithmetically (i.e do not use 2*2)
gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X > Y, !, Z is X mod Y, gcd(Y, Z, D).
gcd(X, Y, D):- Z is Y mod X, gcd(X, Z, D).

% debug/2 is used to output messages with special formatting, only when requested by the user.
% It is SWI-Prolog specific.
% This is a simplified version that will work properly on all Prologs.

:- dynamic cl_debug_mode.
cl_debug_mode :- fail.

debugmsg(Tokens) :-
	cl_debug_mode -> (
		write(user_error,'[01;34m[DEBUG][00m '),
		debug_print(Tokens)
	); true.

debug_print([]) :- nl.
debug_print([Token|TokenS]) :-
	write(user_error,Token),
	debug_print(TokenS).


% Some Prologs only support call/1. This predicate uses Univ to convert a list of arguments into a term, which is then called.
%
call_list(List) :-
	List =.. Term,
	call(Term).

% vi: ft=prolog
