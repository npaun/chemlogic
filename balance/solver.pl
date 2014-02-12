:- use_module(library(clpq)).


equation_system([],_,[]).

equation_system([H|T],Vars,[Equation|EquationS]) :-
        equation_coefficients(H,Vars,LTerms),
        equation_terms_pretty(LTerms,LHS),
        !,
        Equation =.. [=,LHS,0],
        equation_system(T,Vars,EquationS).

% Evaluate

equation_system_eval([],_).

equation_system_eval([Equation|EquationS],VarS) :-
	{Equation},
	equation_system_eval(EquationS,VarS).



% Specify the base condition
equation_evaluate(Matrix,Solution) :-
	VarS = [FirstVar|_],
	equation_system(Matrix,VarS,System),
	require_positive(VarS),
	equation_system_eval(System,VarS),
	bb_inf(VarS,FirstVar,_,Solution),
	!.

% Assign a variable to each coefficient

require_positive([]).

require_positive([Var|VarS]) :-
	{Var > 0},
	require_positive(VarS).

equation_coefficients([],[],[]).

equation_coefficients([Coeff|CoeffS],[Var|VarS],[Term|TermS]) :-
	Term =.. [*,Coeff,Var],
	equation_coefficients(CoeffS,VarS,TermS).

%%% Test Case %%%

test(Values) :-
	equation_evaluate([[1,0,-1,0],[4,0,0,-2],[0,2,-2,-1]],Values).


equation_terms_pretty([Result|[]],Result) :- !.

equation_terms_pretty([L,R|Rest],Result) :-
        Sum = L + R,
        equation_terms_pretty([Sum|Rest],Result).

