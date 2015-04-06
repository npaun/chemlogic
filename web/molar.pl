% molar.pl: Web frontend for the stoichometric unit conversion features
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun




:- http_handler('/chemlogic/molar', molar_page, []).



molar_input(Request,Type,Input) :-
	
	http_parameters(Request,
		[   
			type(Type, [ optional(true), oneof([name,formula]) ]),
			molar_input(Input, [ optional(true) ])
		]).

molar_html(Type,Input,Solution) :-
( var(Input) -> Input = []; true),
(Type = name -> 
	SelectList = [option([value(name),selected],'Name'),option([value(formula)],'Formula')];
	SelectList = [option([value(name)],'Name'),option([value(formula),selected],'Formula')] 
	),


	reply_html_page(chemlogic,title('Molar'),
		[
			h1(id(feature),'Molar'),
			form([
				select(name(type),SelectList),
				input([name(molar_input),id(molar_input),type(text),size(60),value(Input)])
			]),
			div(id(solution),Solution)
		]
		).

molar_nop(Solution) :-
	Solution = 'Please select Name or Formula, depending on what you are entering, and then enter it into the textbox.'.	


molar_process(Type,Input,Solution) :-
	atom_chars(Input,StringInput),
	(molar_do_process(Type,StringInput,StringSolution), chemweb_to_html(StringSolution,Solution)) handle Solution.

molar_do_process(name,Name,Formula) :-
	name_2_formula(Name,Formula).

molar_do_process(formula,Formula,Name) :-
	formula_2_name(Formula,Name).


molar_page(Request) :-
	molar_input(Request,Type,Input),
	(nonvar(Input) -> molar_process(Type,Input,Solution); molar_nop(Solution)),

	molar_html(Type,Input,Solution).
