:- module(web_error,[parse_error_debug/4,highlight_error/3,message_show/1,error_status/0]).

parse_error_debug(Unparsed,Token,ErrCode,TokenType) :-
	 format('Content-type: text/html~n~n', []),
	 postage(ErrCode,'test',[]).

postage(Unparsed) -->
	 html_post(postage,
	 h1(Unparsed)).

highlight_error(Start,Token,Rest) :-
	writeln(Rest).

message_show(Message) :-
	writeln(Message).

error_status.
