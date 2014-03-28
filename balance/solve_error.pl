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
