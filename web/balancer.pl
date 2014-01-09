
:- http_handler('/chemlogic/balancer', balancer_page, []).

balancer_input(Request,Type,Input,OutputType) :-
	
	http_parameters(Request,
		[   
			type(Type, [ optional(true), oneof([symbolic,word]) ]),
			input(Input, [ optional(true) ]),
			outputtype(OutputType, [ optional(true), oneof([symbolic,word]) ])
		]).

balancer_html(Type,Input,OutputType,Solution) :-
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
				input([name(input),id(input),type(text),size(80),value(Input)]),
				select(name(outputtype),OutputSelectList)
			]),
			p(id(solution),\[Solution])
		]
		).

balancer_nop(Solution) :-
	Solution = 'Please select Symbolic or Word equation, depending on what you are entering, then enter it into the textbox. You can also select how the equation will be output.'.	


balancer_process(Type,Input,OutputType,Solution) :-
	atom_chars(Input,StringInput),
	balancer_do_process(Type,StringInput,OutputType,StringSolution),
	atom_chars(Solution,StringSolution).

balancer_do_process(Type,StringInput,OutputType,StringSolution) :-
	balance_equation(Type,StringInput,OutputType,StringSolution);
	StringSolution = "<span class='failed'>FAILED: Balancing of equation.</span> Unfortunately, a detailed reason is not yet available.".

balancer_page(Request) :-
	balancer_input(Request,Type,Input,OutputType),
	(nonvar(Input) -> balancer_process(Type,Input,OutputType,Solution); balancer_nop(Solution)),

	balancer_html(Type,Input,OutputType,Solution).
