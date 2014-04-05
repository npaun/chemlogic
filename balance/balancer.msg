


%%%%% GUIDANCE FOR ERRORS %%%%%



guidance_process(equation_struct,
	'Your equation completely follows the rules for chemical equations,
       	 but the information from the equation could not be used to make a
 	 lookup table. The two formats may be incompatible for some reason.	 
	
	 INTERNAL LOGIC ERROR --- this is probably a bug in the program.

 	 Equation in internal format: ').

guidance_process(data_sets,
	'Your equation completely follows the rules for chemical equations,
	 but the information collected while processing the equation could
	 not be used to construct a matrix.
	
 	 INTERNAL LOGIC ERROR --- this is probably a bug in the program.

	The collected data sets: ').

explain_data([],[]).
% vi: ft=prolog

