% balancer.pl: Web frontend for the chemical equation balancer
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- http_handler('/chemlogic/',balancer_page,[]). %% Make it the default page
:- http_handler('/chemlogic/balancer', balancer_page, []).



balancer_input(Request,Type,Input,OutputType) :-
	http_parameters(Request,
	[
	type(Type, [ optional(true), oneof([symbolic,word]) ]),
	balancer_input(Input, [ optional(true) ]),
	outputtype(OutputType, [ optional(true), oneof([symbolic,word]) ])
	]).

balancer_html(Type,Input,OutputType,Solution) :-
	( var(Input) -> Input = []; true),
	OptionS = [[symbolic,'Symbolic'],[word,'Word']],
	chemweb_select_list(Type,OptionS,SelectList),
	chemweb_select_list(OutputType,OptionS,OutputSelectList),

	reply_html_page(chemlogic,title('Balancer'),
	[
	h1(id(feature),'Balancer'),
	form([
	select(name(type),SelectList),
	input([name(balancer_input),id(balancer_input),type(text),size(80),value(Input)]),
	select(name(outputtype),OutputSelectList)
	]),
	div(id(solution),Solution)
	]
	).

balancer_nop(Solution) :-
	Solution = 'Please select Symbolic or Word equation, depending on what you are entering, then enter it into the textbox. You can also select how the equation will be output.'.


balancer_process(Type,Input,OutputType,Solution) :-
	atom_chars(Input,StringInput),
	balancer_do_process(Type,StringInput,OutputType,Solution).

balancer_do_process(Type,StringInput,OutputType,Solution) :-
	(balance_equation(Type,StringInput,OutputType,StringSolution), chemweb_to_html(StringSolution,Solution)) handle Solution.


balancer_page(Request) :-
	balancer_input(Request,Type,Input,OutputType),
	(nonvar(Input) -> balancer_process(Type,Input,OutputType,Solution); balancer_nop(Solution)),

	balancer_html(Type,Input,OutputType,Solution).
