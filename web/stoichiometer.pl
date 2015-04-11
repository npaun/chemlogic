% stoichiometer.pl: Web frontend for the stoichiometric calculator
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- http_handler('/chemlogic/stoichiometer', stoichiometer_page, []).



stoichiometer_input(Request,Type,Input,OutputType,QueryS) :-
	http_parameters(Request,
	[
	type(Type, [ optional(true), oneof([symbolic,word]) ]),
	stoichiometer_input(Input, [ optional(true) ]),
	outputtype(OutputType, [ optional(true), oneof([symbolic,word]) ]),
	queries_input(QueryS, [list(atom) ])
	]).

stoichiometer_html(Type,Input,OutputType,Solution,QueryS) :-
	( var(Input) -> Input = []; true),
	OptionS = [[symbolic,'Symbolic'],[word,'Word']],
	chemweb_select_list(Type,OptionS,SelectList),
	chemweb_select_list(OutputType,OptionS,OutputSelectList),

	reply_html_page(chemlogic,title('Stoichiometer'),
	[
	h1(id(feature),'Stoichiometer'),
	form([
	select(name(type),SelectList),
	input([name(stoichiometer_input),id(stoichiometer_input),type(text),size(80),value(Input)]),
	select(name(outputtype),OutputSelectList)
	]),
	div(id(solution),Solution)
	]
	).

stoichiometer_nop(Solution) :-
	Solution = '(1) Please select Symbolic or Word equation, depending on what you are entering, then enter it into the textbox. You can also select how the equation will be output. (2) After pressing ENTER, select the quantities you wish to determine from the dropdown menus.'.


stoichiometer_queries_create([],[]).
stoichiometer_queries_create([Unit|UnitS],[Query|QueryS]) :-
	(Unit = nil ->
		Query = nil;
		Query = [[[[_, _], Unit]], actual]
	),
	stoichiometer_queries_create(UnitS,QueryS).


stoichiometer_process(Type,Input,OutputType,Solution,UnitS) :-
	atom_chars(Input,StringInput),
	stoichiometer_queries_create(UnitS,QueryS),
	stoichiometer_do_process(Type,StringInput,OutputType,Solution,QueryS).

stoichiometer_do_process(Type,StringInput,OutputType,Solution,QueryS) :-
	(stoich_queries(Type,StringInput,OutputType,StringSolution,QueryS), chemweb_to_html(StringSolution,Solution)) handle Solution.


stoichiometer_page(Request) :-
	stoichiometer_input(Request,Type,Input,OutputType,QueryS),
	(nonvar(Input) -> stoichiometer_process(Type,Input,OutputType,Solution,QueryS); stoichiometer_nop(Solution)),

	stoichiometer_html(Type,Input,OutputType,Solution,QueryS).
