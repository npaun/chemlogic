:- module(cli_error,[error_handler/2]).


highlight_error(highlight(Start,Token,Rest)) :-
	writef('%s\e[01;41;37m%s\e[00m%s\n',[Start,Token,Rest]).

message_show(message(MessageErrcode,MessageUnparsed,ErrCode)) :-
	writeln(MessageErrcode),nl,
	write(MessageUnparsed), writeln(ErrCode),nl.

error_handler(_,[HighlightStruct,MessageStruct]) :-
	highlight_error(HighlightStruct),
	message_show(MessageStruct),
	fail.
