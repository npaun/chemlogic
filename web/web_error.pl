:- module(web_error,[error_handler/2]).

highlight_error(highlight(Start,Token,Rest),HighlightHTML) :-
	HighlightHTML = p(class('error-line'),
		[
			span(Start),
			span(class('error-token'),Token),
			span(Rest)
		]
	).

message_show_syntax(message(MessageErrcode,MessageUnparsed,Errcode),MessageHTML) :-
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

message_show_process(message(Message,Data),MessageHTML) :-
	MessageHTML = div(class('error-message'),
		[
			pre(
				[
					Message,
					span(class(errcode),Data)
				]
			)
		]
	).


error_handler([HighlightHTML,MessageHTML],syntax_error([HighlightStruct,MessageStruct])) :-
	highlight_error(HighlightStruct,HighlightHTML),
	message_show_syntax(MessageStruct,MessageHTML).

error_handler([HighlightHTML,MessageHTML],type_error([HighlightStruct,MessageStruct])) :-
	highlight_error(HighlightStruct,HighlightHTML),
	message_show_process(MessageStruct,MessageHTML).
