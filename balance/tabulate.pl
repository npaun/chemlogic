% tabulate.pl: Builds a lookup table, implemented as a dynamic predicate for a parsed equation struct
% This file is from Chemlogic, a logic programming computer chemistry system°% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun



:- module(tabulate,[tabulate/1]).
:- dynamic(user:balance/3).



/**
add_atom(+Formula,+Sym,+Value) is det.

Adds Value (an element subscript) to the total count of Sym (an element) in Formula.
Used in flattening polyatomic groups, normalizing formulas with recurrences, etc.

**/
add_atom(Formula,Sym,Value) :-
	(retract(user:balance(Formula,Sym,OldValue)) -> NewValue is OldValue + Value; NewValue = Value),
	assertz(user:balance(Formula,Sym,NewValue)).


/**
tabulate_part(+Formula,+Symbol,+Subscript,+Sign) is det.

1. Traverses a polyatomic group, which is represented as a symbol which is actually a list containing Symbols and Subscripts. It distributes the subscript of the polyatomic group (GroupSub) and applies the sign representing side.
2. Stores a simple pair, a real Sym and Sub (like H, 2). It applies the sign before storage. This is done simply to make the output nicer; store_balance could simply multiply by sign.

@arg Formula	The complete formula that we are processing a part of	Pb(NO3)4
@arg Sym	An element symbol	Pb
@arg Sub	A subscript in a chemical formula	3 as in O3
@arg PairS	The remaining pairs of symbol and subscript	[O,3], etc.
@arg GroupSub	The subscript of th entire polyatomic group	4 as in (NO3)4
@arg Sign	The sign of the entire expression (right hand side is negative)
*/

% 1
tabulate_part(_,[],_,_).

tabulate_part(Formula,[[Sym,Sub]|PairS],GroupSub,Sign) :-
	add_atom(Formula,Sym,Sub * GroupSub * Sign),
	tabulate_part(Formula,PairS,GroupSub,Sign).

% 2
tabulate_part(Original,Sym,Sub,1) :-
	add_atom(Original,Sym,Sub).
tabulate_part(Original,Sym,Sub,-1) :-
	add_atom(Original,Sym,-Sub).



/** formula(+Formula,+Formula,+Sign) is det.

Breaks up a Formula into PairS of Symbol and Subscript, which are then given to store_subpart, which flattens polyatomic groups and applies the sign.

*/

formula(_,[],_) :- !.

formula(Formula,[[Sym,Sub]|PairS],Sign) :-
	tabulate_part(Formula,Sym,Sub,Sign),
	!,
	formula(Formula,PairS,Sign).

expression([],_) :- !.

expression([Formula|FormulaS],Sign) :-
	formula(Formula,Formula,Sign),
	expression(FormulaS,Sign).


/** tabulate(+Tree) is det.

Converts an abstract syntax tree, produced by the parsing of a chemical equation.
(Something like: CH4, O2; CO2, H2O)

into a normalized series of facts allowing easy lookups by element name in each formula.
(Something like:

CH4	C	1
CH4	H	4
O2	O	2
etc etc.
)

Note that this procedure causes the Products (the Right Hand Side of the equation) to be given a negative sign. This makes later procedures much simpler because there is no need to track which variables are on each side of the equation; this way the variables are all on the left, equal to 0.

FOR PERFORMANCE REASONS ONLY.
*/


tabulate([Reactants,Products]) :-
	expression(Reactants,1),
	expression(Products,-1).
