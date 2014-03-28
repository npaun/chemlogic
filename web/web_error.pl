:- module(web_error,[error_handler/2]).

highlight_error(highlight(Start,Token,Rest),HighlightHTML) :-
	HighlightHTML = p(id(errorline),[span(Start),span(id(errortoken),Token),span(Rest)]).

message_show(message(MessageErrcode,MessageUnparsed,Errcode),MessageHTML) :-
	MessageHTML = div(id(errormessage),[pre(MessageErrcode),pre([MessageUnparsed,span(id(errcode),Errcode)])]).

error_handler([HighlightHTML,MessageHTML],[HighlightStruct,MessageStruct]) :-
	highlight_error(HighlightStruct,HighlightHTML),
	message_show(MessageStruct,MessageHTML).
