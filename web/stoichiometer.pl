% stoichiometer.pl: Web frontend for the stoichiometric calculator
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- http_handler('/chemlogic/stoichiometer', stoichiometer_page, []).



stoichiometer_input(Request,Type,Input,OutputType) :-
	http_parameters(Request,
	[
	type(Type, [ optional(true), oneof([symbolic,word]) ]),
	stoichiometer_input(Input, [ optional(true) ]),
	outputtype(OutputType, [ optional(true), oneof([symbolic,word]) ])
	]).

stoichiometer_html(Type,Input,OutputType,Solution) :-
( var(Input) -> Input = []; true),
(Type = symbolic ->
	SelectList = [option([value(symbolic),selected],'Symbolic'),option([value(word)],'Word')];
	SelectList = [option([value(symbolic)],'Symbolic'),option([value(word),selected],'Word')]
	),


(OutputType = symbolic ->
	OutputSelectList = [option([value(symbolic),selected],'Symbolic'),option([value(word)],'Word')];
	OutputSelectList = [option([value(symbolic)],'Symbolic'),option([value(word),selected],'Word')]
	),
	reply_html_page(chemlogic,title('Balancer'),
	[
	h1(id(feature),'Balancer'),
	form([
	select(name(type),SelectList),
	input([name(stoichiometer_input),id(stoichiometer_input),type(text),size(80),value(Input)]),
	select(name(outputtype),OutputSelectList)
	]),
	div(id(solution),Solution)
	]
	).

stoichiometer_nop(Solution) :-
	Solution = 'Please select Symbolic or Word equation, depending on what you are entering, then enter it into the textbox. You can also select how the equation will be output.'.


stoichiometer_process(Type,Input,OutputType,Solution) :-
	atom_chars(Input,StringInput),
	stoichiometer_do_process(Type,StringInput,OutputType,Solution).

stoichiometer_do_process(Type,StringInput,OutputType,Solution) :-
	(balance_equation(Type,StringInput,OutputType,StringSolution), chemweb_to_html(StringSolution,Solution)) handle Solution.


stoichiometer_page(Request) :-
	stoichiometer_input(Request,Type,Input,OutputType),
	(nonvar(Input) -> stoichiometer_process(Type,Input,OutputType,Solution); stoichiometer_nop(Solution)),

	stoichiometer_html(Type,Input,OutputType,Solution).
