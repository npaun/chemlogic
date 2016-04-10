% molar.pl: Web frontend for the stoichometric unit conversion features
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun




:- http_handler('/chemlogic/molar', molar_page, []).



molar_input(Request,Type,Input,Unit,TailInput) :-
	
	http_parameters(Request,
		[   
			type(Type, [ optional(true), oneof([name,formula]) ]),
			molar_input(Input, [ optional(true) ]),
			unit(Unit, [ optional(true), oneof(g,'L(g)','L(sol)',mol,'M') ]),
			molar_tail_input(TailInput, [ optional(true) ])
		]).

molar_html(Type,Unit,Input,TailInput,Solution) :-
	( var(Input) -> Input = []; true),
	( var(TailInput) -> TailInput = []; true),
	chemweb_select_list(Type,[[name,'Name'],[formula,'Formula']],SelectList),
	chemweb_select_list(Unit,[[g,g],['L(g)','L (gas)'],['L(sol)','L (sol.)'],[mol,mol],['M','M']],UnitSelectList),

	reply_html_page(chemlogic,title('Molar'),
		[
			h1(id(feature),'Molar'),
			form([
				select([name(type),id(molar_type)],SelectList),
				input([name(molar_input),id(molar_input),type(text),size(60),value(Input),required]),
				select([name(unit),id(molar_unit)],UnitSelectList),
				\[' ('],
				input([name(molar_tail_input),id(molar_tail_input),type(text),size(10),value(TailInput)]),
				\[')'],
				input([type(submit),class(hidden)])
			]),
			div(id(solution),Solution)
		]
		).

molar_nop(Solution) :-
	Solution = \['Please select the Name or Formula, depending on the format of your input. In the textbox, enter the quantity followed by the name/formula. Then, select the unit you wish to convert to.<br>When converting to a volume or concentration of a solution, you must specify the other property in the textbox in parentheses.'].

molar_truncate_L('Lgas','L').
molar_truncate_L('Laq','L').
molar_truncate_L(X,X).


molar_process(Type,Input,Solution,Unit,QtyTail) :-
	molar_truncate_L(Unit,UnitReal),
	(nonvar(QtyTail) ->
		(
			atom_chars(QtyTail,TailChars),
			parse(quantity([TailStruct]),TailChars,[]) handle Solution,
			Query = [[[[_, _], UnitReal],TailStruct], actual]
		);
		Query = [[[[_, _], UnitReal]], actual]
	),
	(var(Solution) -> 
		(
			atom_chars(Input,StringInput),
			((molar_do_process(Type,StringInput,StringSolution,Query), chemweb_to_html(StringSolution,Solution)) handle Solution)
		);
		true
	).

molar_do_process(name,Name,Formula,Query) :-
	calc_name_2_formula(Name,Formula,Query).

molar_do_process(formula,Formula,Name,Query) :-
	calc_formula_2_name(Formula,Name,Query).


molar_page(Request) :-
	molar_input(Request,Type,Input,Unit,TailInput),
	(nonvar(Input) -> molar_process(Type,Input,Solution,Unit,TailInput); molar_nop(Solution)),
	molar_html(Type,Unit,Input,TailInput,Solution).
