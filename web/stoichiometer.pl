% stoichiometer.pl: Web frontend for the stoichiometric calculator
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- http_handler('/chemlogic/stoichiometer', stoichiometer_page, []).



stoichiometer_input(Request,Type,Input,OutputType,UnitS,PropertyS) :-
	http_parameters(Request,
	[
	type(Type, [ optional(true), oneof([symbolic,word]) ]),
	stoichiometer_input(Input, [ optional(true) ]),
	outputtype(OutputType, [ optional(true), oneof([symbolic,word]) ]),
	units_input(UnitS, [list(atom) ]),
	properties_input(PropertyS, [list(atom)])
	]).

stoichiometer_html(Type,Input,OutputType,Solution,UnitS,PropertyS,ResultS,Info) :-
	( var(Input) -> Input = []; true),
	OptionS = [[symbolic,'Symbolic'],[word,'Word']],
	chemweb_select_list(Type,OptionS,SelectList),
	chemweb_select_list(OutputType,OptionS,OutputSelectList),
	stoichiometer_query_list(UnitS,PropertyS,QuerySelect),
	reply_html_page(chemlogic,title('Stoichiometer'),
	[
	h1(id(feature),'Stoichiometer'),
	form([
	select(name(type),SelectList),
	input([name(stoichiometer_input),id(stoichiometer_input),type(text),size(80),value(Input)]),
	select(name(outputtype),OutputSelectList),
	div(id(solution),Solution),
	div(id(select),QuerySelect)
	]),
	div(id(results),ResultS),
	ul(id(info),Info)
	]
	).

stoichiometer_nop(Solution) :-
	Solution = '(1) Please select Symbolic or Word equation, depending on what you are entering, then enter it into the textbox. You can also select how the equation will be output. (2) After pressing ENTER, select the quantities you wish to determine from the dropdown menus.'.


stoichiometer_query_list([],[],[]).
stoichiometer_query_list([Unit|UnitS],[Property|PropertyS],[Select|SelectS]) :-
		chemweb_select_list(Unit,[[nil,''],[g,g],['L','L'],[mol,mol]],UnitSelectList),
		chemweb_select_list(Property,[[actual,''],[reacted,reacted],[produced,produced],[excess,excess]],PropertySelectList),
		Select = span([select(name(units_input),UnitSelectList),select(name(properties_input),PropertySelectList)]),
		stoichiometer_query_list(UnitS,PropertyS,SelectS).

stoichiometer_queries_create([],[],[]).
stoichiometer_queries_create([Unit|UnitS],[Property|PropertyS],[Query|QueryS]) :-
	(Unit = nil ->
		Query = nil;
		Query = [[[[_, _], Unit]], Property]
	),
	stoichiometer_queries_create(UnitS,PropertyS,QueryS).

stoichiometer_results(true,[],[]).
stoichiometer_results(true,[Query|QueryS],[Result|ResultS]) :-
	(Query = nil ->
		Result = span(\['&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;']);
		(
			parse(query_result(Query),ResultString,[]),
			atom_chars(ResultAtom,ResultString),
			Result = span(['= ',ResultAtom])

		)
	),
	stoichiometer_results(true,QueryS,ResultS).

stoichiometer_results(_,_,_).


stoichiometer_process(Type,Input,OutputType,Solution,UnitS,PropertyS,ResultS,Info) :-
	atom_chars(Input,StringInput),
	stoichiometer_queries_create(UnitS,PropertyS,QueryS),
	stoichiometer_do_process(Type,StringInput,OutputType,Solution,QueryS,Info,OK),
	debug(chemlogic,'~w~n',QueryS-OK),
	stoichiometer_results(OK,QueryS,ResultS).

stoichiometer_do_process(Type,StringInput,OutputType,Solution,QueryS,Info,OK) :-
	(
		stoich_queries(Type,StringInput,OutputType,StringSolution,Struct,QueryS), 
		balancer_do_info(Struct,Info), 
		chemweb_to_html(StringSolution,Solution)
	) handle Solution,
	(var(StringSolution) ->  OK = fail; OK = true).

stoichiometer_nop_select(Type,Input,OutputType,Solution,QtyS,QtyS) :-
	atom_chars(Input,StringInput),
	stoichiometer_do_nop_select(Type,StringInput,OutputType,Solution,QtyS).

stoichiometer_do_nop_select(Type,StringInput,OutputType,Solution,QtyS) :-
		(balance_equation(Type,StringInput,OutputType,StringSolution,_,_,_,stoich,QtyS), chemweb_to_html(StringSolution,Solution)) handle Solution.
	


stoichiometer_page(Request) :-
	stoichiometer_input(Request,Type,Input,OutputType,UnitS,PropertyS),
	(\+ UnitS = [] -> 
		(stoichiometer_process(Type,Input,OutputType,Solution,UnitS,PropertyS,ResultS,Info), RUnitS = UnitS, RPropertyS = PropertyS); 
		(nonvar(Input) ->
			(stoichiometer_nop_select(Type,Input,OutputType,Solution,RUnitS,RPropertyS), debug(chemlogic,'~w~n',Solution));
			stoichiometer_nop(Solution)
		)
	),

	stoichiometer_html(Type,Input,OutputType,Solution,RUnitS,RPropertyS,ResultS,Info).
