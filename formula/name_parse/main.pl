:- include('ionic.pl').
:- include('covalent.pl').


/* Common Subroutines */
non_metal(Sym,Name) --> element(Sym,Name), {charge_check(nonmetal,Sym)}.

non_metal_ide(Sym,Base) --> element_base(Sym,Base), "ide", {charge_check(nonmetal,Sym)}.

charge_check(nonmetal,Sym,Charge) :-
        charge(Sym,Charge),
	\+ is_list(Charge),
        Charge < 0.

charge_check(metal,Sym,Charge) :-
        charge(Sym,Charge),
	!,
        (\+ is_list(Charge) -> 
        Charge > 0),
        !.


charge_check(Type,Sym) :- charge_check(Type,Sym,_).

/* Some simple rules for pure substances */
pure(Sym,Rest,F) --> diatomic(Sym,Rest,F).
pure(Sym,Rest,F) --> single_element(Sym,Rest,F).


diatomic([Sym|Rest],Rest,[[Sym,2]]) --> element(Sym,_), {diatomic(Sym)}.
single_element([Sym|Rest],Rest,[[Sym,1]]) --> element(Sym,_).

/* Parse it up! */

name(Sym,Rest,F) --> name_real(Sym,Rest,F); {var(F) -->
	syntax_error('Name. The given compound name could not be parsed. Please verify that you have entered the name correctly. This error could also be triggered by a missing element name in the database.');
	throw('Logic Error: Name. The internal formula could not be converted into a compound name.')}.

name_real(Sym,Rest,F) --> retained(Sym,Rest,F).
name_real(Sym,Rest,F) --> ionic(Sym,Rest,F).
name_real(Sym,Rest,F) --> covalent(Sym,Rest,F).
name_real(Sym,Rest,F) --> pure(Sym,Rest,F).
name_real(Sym,Rest,F) --> common(Sym,Rest,F).

/** TODO:

The ionic parser messes up when given a non-ionic compound --- it throws an exception.
We need to get rid of the exception and allow priority to ionic names (as an acid instead of hydrogen chloride).

Let's think about it.
**/
