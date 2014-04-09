:- module(cli_error,[error_handler/2]).


highlight_error(highlight(Start,Token,Rest)) :-
	writef('%s\e[01;41;37m%s\e[00m%s\n',[Start,Token,Rest]).

message_syntax_show(message(MessageErrcode,MessageUnparsed,ErrCode)) :-
	writeln(MessageErrcode),nl,
	write(MessageUnparsed), writeln(ErrCode),nl.

message_general_show(message(Message,Data)) :-
	write(Message), writeln(Data), nl.

error_handler(_,syntax_error([HighlightStruct,MessageStruct])) :-
	highlight_error(HighlightStruct),
	message_syntax_show(MessageStruct),
	fail.

error_handler(_,domain_error(Struct)) :- error_handler(_,type_error(Struct)).

error_handler(_,type_error([HighlightStruct,MessageStruct])) :-
	highlight_error(HighlightStruct),
	message_general_show(MessageStruct),
	fail.
