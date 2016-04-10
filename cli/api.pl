% api.pl: Configures the CLI interface to produce machine-readable output for embedded applications
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- consult('../build/compile.cf').




cli_api_init([]) :- cli_api_init([user]).
cli_api_init([Format]) :-
        set_output_format(Format),
        set_stream(user_input,tty(true)),
        set_stream(user_output,tty(true)),
        set_prolog_flag(toplevel_prompt,'CL ?-[~!]\n').

cli_api_message(Format) :-
        writeln('Chemlogic	API		1'),
	(catch(cf_version(Version),_,false); Version = '*** DEVELOPMENT COPY ***'),
	write('"		version		'), writeln(Version),
	db_version(DBVersion),
	write('" database	version		'),writeln(DBVersion),
	(Format = [] -> FormatInfo = '*** DEFAULT ***'; Format = [FormatInfo]),
	write('output		format		'),writeln(FormatInfo),
	current_prolog_flag(version_data,PrologVersion),
	write('Prolog		version		'), writeln(PrologVersion),
	current_prolog_flag(arch,SystemArch),
	write('System		architecture	'),writeln(SystemArch),
	nl.
