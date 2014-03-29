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
			div(id(solution),Solution)
		]
		).

compounder_nop(Solution) :-
	Solution = 'Please select Name or Formula, depending on what you are entering, and then enter it into the textbox.'.	


compounder_process(Type,Input,Solution) :-
	atom_chars(Input,StringInput),
	(compounder_do_process(Type,StringInput,StringSolution), chemweb_to_html(StringSolution,Solution)) handle Solution.

compounder_do_process(name,Name,Formula) :-
	name_2_formula(Name,Formula).

compounder_do_process(formula,Formula,Name) :-
	formula_2_name(Formula,Name).


compounder_page(Request) :-
	compounder_input(Request,Type,Input),
	(nonvar(Input) -> compounder_process(Type,Input,Solution); compounder_nop(Solution)),

	compounder_html(Type,Input,Solution).
