:- module(cli_error,[parse_error_debug/4, highlight_error/3, message_show/1]).

parse_error_debug(Unparsed,Token,ErrCode,TokenType) :- 
	write('Scan '), writeln(Unparsed), 
	write(ErrCode), write(': '), write(Token), write(' is '), writeln(TokenType).

highlight_error(Start,Token,Rest) :-
	writef('%s\e[01;41;37m%s\e[00m%s\n',[Start,Token,Rest]).

message_show(Message) :- writeln(Message).
