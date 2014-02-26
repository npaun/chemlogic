% IDEA:
% Different input fields for names, formulas, symbolic equations and word equations. This would make auto-complete more useful, perhaps.

:- http_handler('/chemlogic/compounder', compounder_page, []).

compounder_input(Request,Type,Input) :-
	
	http_parameters(Request,
		[   
			type(Type, [ optional(true), oneof([name,formula]) ]),
			input(Input, [ optional(true) ])
		]).

compounder_html(Type,Input,Solution) :-
( var(Input) -> Input = []; true),
(Type = name -> 
	SelectList = [option([value(name),selected],'Name'),option([value(formula)],'Formula')];
	SelectList = [option([value(name)],'Name'),option([value(formula),selected],'Formula')] 
	),


	reply_html_page(chemlogic,title('Compounder'),
		[
			h1(id(feature),'Compounder'),
			form([
				select(name(type),SelectList),
				input([name(input),id(input),type(text),size(60),value(Input)])
			]),
			p(id(solution),\[Solution])
		]
		).

compounder_nop(Solution) :-
	Solution = 'Please select Name or Formula, depending on what you are entering, and then enter it into the textbox.'.	


compounder_process(Type,Input,Solution) :-
	atom_chars(Input,StringInput),
	compounder_do_process(Type,StringInput,StringSolution),
	atom_chars(Solution,StringSolution).

compounder_do_process(name,Name,Formula) :-
	name(Elements,[],Internal,Name,[]) -> 
		(
			formula(output,Elements,[],Internal,Formula,[]) -> true;
			Formula = "<span class='failed'>FAILED: Conversion of processed name to a formula.</span> This is a bug in the program."
		);
	Formula = "<span class='failed'>FAILED: Processing of compound name.</span> The program may not have information on the elements/groups used in your compound. But, please verify that you have not misentered anything.".

compounder_do_process(formula,Formula,Name) :-
        formula(user,Elements,[],Internal,Formula,[]) -> 
                (
                        name(Elements,[],Internal,Name,[]) -> true;
                        Name = "<span class='failed'>LOGIC ERROR: Conversion of processed formula to name.</span> This probably a bug in the program, but may reflect a missing polyatomic ion in the database."
                );
        Name = "<span class='failed'>FAILED: Processing of formula.</span> The program may not have information on the elements/groups used in your compound. But, please verify that you have not misentered anything.".
	

compounder_page(Request) :-
	compounder_input(Request,Type,Input),
	(nonvar(Input) -> compounder_process(Type,Input,Solution); compounder_nop(Solution)),

	compounder_html(Type,Input,Solution).
