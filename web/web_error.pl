:- module(web_error,[error_handler/2]).

highlight_error(highlight(Start,Token,Rest),HighlightHTML) :-
	HighlightHTML = p(class('error-line'),
		[
			span(Start),
			span(class('error-token'),Token),
			span(Rest)
		]
	).

message_show(message(MessageErrcode,MessageUnparsed,Errcode),MessageHTML) :-
	MessageHTML = div(class('error-message'),
		[
			pre(MessageErrcode),
			pre(
				[
					MessageUnparsed,
					span(class(errcode),Errcode)
				]
			)
		]
	).

error_handler([HighlightHTML,MessageHTML],[HighlightStruct,MessageStruct]) :-
	highlight_error(HighlightStruct,HighlightHTML),
	message_show(MessageStruct,MessageHTML).
