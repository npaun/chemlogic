:- include('ionic.pl').
:- include('covalent.pl').


/* Common Subroutines */
nonmetal(Sym,Name,Charge) --> element(Sym,Name), {charge_check(nonmetal,Sym,Charge)}.


nonmetal_ide(Sym,Base,Charge) --> 
	element_base(Sym,Base), 
	({charge_check(nonmetal,Sym,Charge)}; syntax_stop(nonmetal)),
	("ide"; syntax_stop(ide)).

/** charge_check(+Type,+Sym,?Charge) is semidet.
 ** charge_check(+Type,+Sym) is semidet.

Tests whether a given symbol is a nonmetal or metal. This is used in parsers to ensure that the user supplies a two nonmetals in the systematic_covalent parser or that a compound consists of a metal and a nonmetal in the ionic parser, for example.

It can also return the charge, because we need it to do the test anyway.

@vague	Type
@arg	Type	The type of element/polyatomic group Sym is supposed to be.	(nonmetal or metal).
@arg	Sym	An element/polyatomic groups symbol/internal formula.	Ag
@arg	Charge	An ionic charge or list of charges for an element or polyatomic ion	-1, [2,3], etc.
@todo	Include a test that verifies that Charge is really a Charge of a polyatomic group.

 */

charge_check(nonmetal,Sym,Charge) :-
        charge(Sym,Charge),
	\+ is_list(Charge),
        Charge =< 0.

charge_check(metal,Sym,Charge) :-
        charge(Sym,Charge),
	!,
        (\+ is_list(Charge) ->
        Charge > 0),
        !.

charge_check(Type,Sym) :- charge_check(Type,Sym,_).

/* Some simple rules for pure substances */
pure(Sym,Rest,Formula) --> diatomic(Sym,Rest,Formula).
pure(Sym,Rest,Formula) --> single_element(Sym,Rest,Formula).


diatomic([Sym|Rest],Rest,[[Sym,2]]) --> element(Sym,_), {diatomic(Sym)}.
single_element([Sym|Rest],Rest,[[Sym,1]]) --> element(Sym,_).

/* Parse it up! */


name(Sym,Rest,Formula,Name,[]) :- pure(Sym,Rest,Formula,Name,[]).
name(Sym,Rest,Formula) --> retained(Sym,Rest,Formula).
name(Sym,Rest,Formula) --> ionic(Sym,Rest,Formula).
name(Sym,Rest,Formula) --> covalent(Sym,Rest,Formula).
name(Sym,Rest,Formula) --> common(Sym,Rest,Formula).

/** TODO:

The ionic parser messes up when given a non-ionic compound --- it throws an exception.
We need to get rid of the exception and allow priority to ionic names (as an acid instead of hydrogen chloride).

Let's think about it.
**/
