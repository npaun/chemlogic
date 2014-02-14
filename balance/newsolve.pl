:- module(newsolve,[nsystem/4,nsolve/3]).
:- use_module(library(clpq)).

% Build a nsystem of linear equations

/** nsystem(+Matrix,-VarS,-System) is det.

Transforms a matrix into a nsystem of linear equations. Rather inefficient, given that a matrix can be nsolved directly.

@arg	Matrix	A matrix created from the element occurences per molecule.
@arg	Row	A row in that matrix	Carbon = [1 in CH4, 0 in O2, -1 in CO2, -0 H2O], etc.
@arg	VarS	An unstantiated list that will later hold the equation coefficients
@arg	Systems	A nsystem of linear equations

**/



nsystem(_,[],_,[]).

nsystem(MolSet,[Elem|ElemS],VarS,[Equation|EquationS]) :-
        equation_terms(MolSet,Elem,VarS,LTermS),
        equation_expression(LTermS,LHS),
        !,
        Equation =.. [=,LHS,0],
        nsystem(MolSet,ElemS,VarS,EquationS).

equation_terms([],_,[],[]).

equation_terms([Mol|MolS],Elem,[Var|VarS],[Term|TermS]) :-
	get_element_count(Mol,Elem,Count),
	Term =.. [*,Count,Var],
	equation_terms(MolS,Elem,VarS,TermS).


equation_expression([TermS|[]],TermS) :- !.

equation_expression([TermL,TermR|TermS],Expr) :-
        Sum = TermL + TermR,
        equation_expression([Sum|TermS],Expr).


% Evaluate

nsystem_eval([],_).

nsystem_eval([Equation|EquationS],VarS) :-
	{Equation}, % Add Equation to the CLP(Q) constraint store)
	nsystem_eval(EquationS,VarS).


/** nsolve(+Matrix,-Solution) is semidet.

Converts a Matrix describing a chemical equation into a nsystem of linear equations and produces the simplest, positive solution.

@error TODO	If there is no possible solution, this function will fail.
**/

nsolve(MolSet,Elems,Solution) :-
	VarS = [FirstVar|_], % We use the first variable when putting the solution in simplest form
	nsystem(MolSet,Elems,VarS,System),
	require_positive(VarS), % Requires all variables to be positive
	nsystem_eval(System,VarS),
	bb_inf(VarS,FirstVar,_,Solution),% Takes the lowest solution that satisfies all of the constraints.
	!.


/** require_positive(-VarS) is det.

Requires that every variable (representing a chemical equation coefficient) by positive in order for a solution to be valid.
**/

require_positive([]).

require_positive([Var|VarS]) :-
	{Var > 0},
	require_positive(VarS).

%%% Test Case %%%

test(Values) :-
	equation_evaluate([[1,0,-1,0],[4,0,0,-2],[0,2,-2,-1]],Values).

