% cli_error.pl: An error output format for terminals, using ANSI color.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- module(cli_error,[error_handler/2]).



error_line(Start,Token,Rest) --> Start, output(output,err_token_start), Token, output(output,err_token_end), Rest.

highlight_error(highlight(Start,Token,Rest)) :-
	error_line(Start,Token,Rest,String,[]),
	atom_chars(Highlight,String),
	writeln(Highlight).	

message_syntax_show(message(MessageErrcode,MessageUnparsed,ErrCode)) :-
	writeln(MessageErrcode),nl,
	write(MessageUnparsed), writeln(ErrCode), nl.

message_general_show(message(Message,DebugInfo)) :-
	write(Message), writeln(DebugInfo), nl.

error_handler(_,syntax_error([HighlightStruct,MessageStruct])) :-
	highlight_error(HighlightStruct),
	message_syntax_show(MessageStruct),
	fail.

error_handler(_,domain_error(Struct)) :- error_handler(_,type_error(Struct)).

error_handler(_,type_error([HighlightStruct,MessageStruct])) :-
	highlight_error(HighlightStruct),
	message_general_show(MessageStruct),
	fail.
