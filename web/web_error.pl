:- module(web_error,[parse_error_debug/4,error_handler/2]).

parse_error_debug(_,_,_,_) :- print_message(error,).
/*
	  format('Content-type: text/html~n~n', []),
	  format('Junk',[]).
  */

highlight_error(highlight(Start,Token,Rest),HighlightHTML) :-
	HighlightHTML = p(id(errorline),[span(Start),span(id(errortoken),Token),span(Rest)]).

message_show(message(MessageErrcode,MessageUnparsed,Errcode),MessageHTML) :-
	MessageHTML = div(id(errormessage),[pre(MessageErrcode),pre([MessageUnparsed,span(id(errcode),Errcode)])]).

error_handler([HighlightHTML,MessageHTML],[HighlightStruct,MessageStruct]) :-
	highlight_error(HighlightStruct,HighlightHTML),
	message_show(MessageStruct,MessageHTML).
