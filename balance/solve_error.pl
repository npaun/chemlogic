count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

countall(List,X,C) :-
	sort(List,List1),
	member(X,List1),
	count(List,X,C).

solve_explain(Matrix,(Elems,_)) :-
	countall(Elems,Elem,Count),
	Count < 2.


solve_handler(Matrix,Coeffs,Data) :-
	catch(
		solve_run(Matrix,Coeffs,Data),
		error(domain_error(solvable,Matrix),Data),
		solve_explain(Matrix,Data)
	).


solve_run(Matrix,Coeffs,(Elems,MolSet)) :-
	(solve(Matrix,Coeffs);
		throw(error(domain_error(solvable,Matrix),
				(Elems,MolSet)
			)
		)
	).
