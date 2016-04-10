% balancer.pl: Web frontend for the chemical equation balancer
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2016 Nicholas Paun



:- http_handler('/chemlogic/',balancer_page,[]). %% Make it the default page
:- http_handler('/chemlogic/balancer', balancer_page, []).



balancer_input(Request,Type,Input,OutputType) :-
	http_parameters(Request,
	[
	type(Type, [ optional(true), oneof([symbolic,word]) ]),
	balancer_input(Input, [ optional(true) ]),
	outputtype(OutputType, [ optional(true), oneof([symbolic,word]) ])
	]).

balancer_html(Type,Input,OutputType,Solution,Info) :-
	( var(Input) -> Input = []; true),
	OptionS = [[symbolic,'Symbolic'],[word,'Word']],
	chemweb_select_list(Type,OptionS,SelectList),
	chemweb_select_list(OutputType,OptionS,OutputSelectList),

	reply_html_page(chemlogic,title('Balancer'),
	[
	h1(id(feature),'Balancer'),
	form([
	select([name(type),id(balancer_type)],SelectList),
	input([name(balancer_input),id(balancer_input),type(text),size(80),value(Input),required]),
	select(name(outputtype),OutputSelectList)
	]),
	div(id(solution),Solution),
	ul(id(info),Info)
	]
	).

balancer_nop(Solution) :-
	Solution = \['Select the form of your equation &mdash; Word or Symbolic &mdash; then enter it into the textbox. You may also select the format of the balanced equation.'].


balancer_process(Type,Input,OutputType,Solution,TypeInfo) :-
	atom_chars(Input,StringInput),
	balancer_do_process(Type,StringInput,OutputType,Solution,TypeInfo).


balancer_do_process(Type,StringInput,OutputType,Solution,TypeInfo) :-
	(
		balance_equation(Type,StringInput,OutputType,StringSolution,_,_,Struct,disable,_),
		balancer_do_info(Struct,TypeInfo),
		chemweb_to_html(StringSolution,Solution)
	) handle Solution.

balancer_do_info(Struct,[TypeInfo,ReactsInfo]) :-
	(
		reaction_match(Type,Struct,Reacts) ->
			(
				reaction_info(Type,TypeDesc),
				TypeInfo = li([em('Reaction Type: '),TypeDesc]),
				((
					activity_info(Reacts,ReactsDesc),
					ReactsInfo = li([em('Reaction Occurs: '),ReactsDesc])
				); ReactsInfo = \[''])

			);
			true
	).

balancer_page(Request) :-
	balancer_input(Request,Type,Input,OutputType),
	(nonvar(Input) -> balancer_process(Type,Input,OutputType,Solution,Info); balancer_nop(Solution)),

	balancer_html(Type,Input,OutputType,Solution,Info).
