
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



% code_type/2 is not supported by all Prologs. This is a simple re-implementation for the purposes of Chemlogic.

code_type(32 /* SPACE */,white).
code_type(9 /* TAB */, white).

code_type(Code,digit) :-
	Code >= 48 /* 0 */,
	Code =< 57 /* 9 */.

code_type(Code,upper) :-
	Code >= 65 /* A */,
	Code =< 90 /* Z */.

code_type(Code,lower) :-
	Code >= 97 /* a */,
	Code =< 122 /* z */.

code_type(Code,alpha) :- 
	code_type(Code,upper);
	code_type(Code,lower).

code_type(Code,alnum) :-
	code_type(Code,alpha);
	code_type(Code,digit).

% This is not the exact same definition as SWI-Prolog's but it should do for the purpoes of this program.
code_type(Code,punct) :- 
	\+ code_type(Code,alnum),
	\+ code_type(Code,white).	


% vi: ft=prolog
