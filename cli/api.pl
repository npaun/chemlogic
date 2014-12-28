:- consult('../build/compile.cf').

cli_api_init([]) :- cli_api_init([user]).
cli_api_init([Format]) :-
        set_output_format(Format),
        set_stream(user_input,tty(true)),
        set_stream(user_output,tty(true)),
        set_prolog_flag(toplevel_prompt,'CL ?-[~!]\n').

cli_api_message :-
        writeln('Chemlogic	API		1'),
	cf_version(Version),
	write('"		version		'), writeln(Version),
	db_version(DBVersion),
	write('" database	version		'),writeln(DBVersion),
	current_prolog_flag(version_data,PrologVersion),
	write('Prolog		version		'), writeln(PrologVersion),
	current_prolog_flag(arch,SystemArch),
	write('System		architecture	'),writeln(SystemArch),
	nl.
	

