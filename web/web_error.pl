% web_error.pl: Error handling predicates for HTML output, including token highlighting
% This file is from Chemlogic, a logic programming computer chemistry system
% (C) Copyright 2012-2015 Nicholas Paun



:- module(web_error,[error_handler/2]).



highlight_error(highlight(Start,Token,Rest),HighlightHTML) :-
	HighlightHTML = p(class('error-line'),
		[
			span(Start),
			span(class('error-token'),Token),
			span(Rest)
		]
	).

message_syntax_show(message(MessageErrcode,MessageUnparsed,Errcode),MessageHTML) :-
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

message_general_show(message(Message,DebugInfo),MessageHTML) :-
	MessageHTML = div(class('error-message'),
		[
			pre(
				[
					Message,
					span(class(errcode),'~w'-[DebugInfo])
				]
			)
		]
	).


error_handler([HighlightHTML,MessageHTML],syntax_error([HighlightStruct,MessageStruct])) :-
	highlight_error(HighlightStruct,HighlightHTML),
	message_syntax_show(MessageStruct,MessageHTML).

error_handler(HTML,domain_error(Struct)) :- error_handler(HTML,type_error(Struct)).

error_handler([HighlightHTML,MessageHTML],type_error([HighlightStruct,MessageStruct])) :-
	highlight_error(HighlightStruct,HighlightHTML),
	message_general_show(MessageStruct,MessageHTML).
