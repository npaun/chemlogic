:- use_module(library(clpfd)).

% Equate each term to zero to create a system of linear equations
equation_system_live_eval([],_).

equation_system_live_eval([H|T],Vars) :-
	equation_coefficients(H,Vars,LTerms),
	equation_terms_pretty(LTerms,LHS),
	!,
	LHS #= 0,
	equation_system_eval(T,Vars).

% System only
equation_system([],_,[]).

equation_system([H|T],Vars,[Equation|EquationS]) :-
        equation_coefficients(H,Vars,LTerms),
        equation_terms_pretty(LTerms,LHS),
        !,
        Equation =.. [#=,LHS,0],
        equation_system(T,Vars,EquationS).

% Evaluator

equation_system_eval([],_).

equation_system_eval([H|T],Vars) :-
	H,
	equation_system_eval(T,Vars).



% Specify the base condition
equation_evaluate(CoeffS,Values) :-
	Values = [FirstVar,TestVar|_],
	FirstVar #> 0,
	FirstVar #=< 10,
	equation_system(CoeffS,Values,System),
	equation_system_eval(System,Values),
	finite_domain(TestVar) -> label_minimize(FirstVar,Values); pad_out(Values).

label_minimize(FirstVar,Vars) :-
Options = [min(FirstVar)],
clpfd:label(Options, Options, default(leftmost), default(up), default(step), [], upto_ground, Vars).

finite_domain(Var) :-
        (   clpfd:fd_get(Var, Dom, _) ->
            (   clpfd:domain_infimum(Dom, n(_)), clpfd:domain_supremum(Dom, n(_)) -> true
            ;   false
            )
        ;   integer(Var) -> true
        ;   true
        ).



pad_out([]).
pad_out([1|T]) :- pad_out(T).
	

% Add terms together
equation_terms([],0).

equation_terms([H|T],Out) :-
	equation_terms(T,Rest),
	Out = H + Rest.

% Assign a variable to each coefficient

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

