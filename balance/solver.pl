:- use_module(library(clpq)).

equation_system([],_,[]).

equation_system([H|T],Vars,[Equation|EquationS]) :-
        equation_coefficients(H,Vars,LTerms),
        equation_terms_pretty(LTerms,LHS),
        !,
        Equation =.. [=,LHS,0],
        equation_system(T,Vars,EquationS).

% Evaluator

equation_system_eval([],_).

equation_system_eval([H|T],Vars) :-
	{H},
	equation_system_eval(T,Vars).



% Specify the base condition
equation_evaluate(CoeffS,Solution) :-
	Values = [FirstVar|_],
	equation_system(CoeffS,Values,System),
	positive(Values),
%	writeln(System),
	equation_system_eval(System,Values),
	bb_inf(Values,FirstVar,_,Solution),
%	writeln(Inf),
	!.

% Add terms together
equation_terms([],0).

equation_terms([H|T],Out) :-
	equation_terms(T,Rest),
	Out = H + Rest.

% Assign a variable to each coefficient

positive([]).

positive([Var|VarS]) :-
	{Var > 0},
	positive(VarS).

equation_coefficients([],[],[]).

equation_coefficients([Coeff|CoeffS],[Var|VarS],[Term|TermS]) :-
	Term =.. [*,Coeff,Var],
	equation_coefficients(CoeffS,VarS,TermS).


%%% Eval %%%


%%% Test Case %%%

test(Values) :-
	equation_evaluate([[1,0,-1,0],[4,0,0,-2],[0,2,-2,-1]],Values).


equation_terms_pretty([Result|[]],Result) :- !.

equation_terms_pretty([L,R|Rest],Result) :-
        Sum = L + R,
        equation_terms_pretty([Sum|Rest],Result).

