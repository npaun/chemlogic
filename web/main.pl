
:- set_prolog_flag(double_quotes,chars).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- initialization server(8000,[]).
:- include('../balance/balancer').
:- set_prolog_flag(double_quotes,chars).
server(Port, Options) :-
        http_server(http_dispatch,
                    [ port(Port)
                    | Options
                    ]).

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


	reply_html_page(title('Compounder'),
		[
			h1('Compounder'),
			form([
				select(name(type),SelectList),
				input([name(input),id(input),type(text),value(Input)])
			]),
			p(\[Solution])
		]
		).

compounder_nop(Solution) :-
	Solution = 'Welcome. Please select what you are inputting (a name or a formula) and enter it into the text box'.	


compounder_process(Type,Input,Solution) :-
	atom_chars(Input,StringInput),
	compounder_do_process(Type,StringInput,StringSolution),
	atom_chars(Solution,StringSolution).

compounder_do_process(name,Name,Formula) :-
	name(Elements,[],Internal,Name,[]) -> 
		(
			formula(Elements,[],Internal,Formula,[]) -> true;
			Formula = "FAILED: Conversion of processed name to a formula. This is a bug in the program."
		);
	Formula = "FAILED: Processing of compound name. The program may not have information on the elements/ggroups used in your compound. But, please verify that you have not misentered anything.".

compounder_do_process(formula,Formula,Name) :-
        formula(Elements,[],Internal,Formula,[]) -> 
                (
                        name(Elements,[],Internal,Name,[]) -> true;
                        Name = "FAILED: Conversion of processed formula to name. This probably a bug in the program, but may reflect a missing polyatomic ion in the database."
                );
        Name = "FAILED: Processing of formula . The program may not have information on the elements/groups used in your compound. But, please verify that you have not misentered anything.".
	

compounder_page(Request) :-
	compounder_input(Request,Type,Input),
	(nonvar(Input) -> compounder_process(Type,Input,Solution); compounder_nop(Solution)),

	compounder_html(Type,Input,Solution).
