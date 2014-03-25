:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes,chars).

scan_rule([C|T]) -->
	        [C],
		        {\+ char_type(C,white)}, !,
			        scan_rule(T).

			scan_rule([]) -->
				        [].

token_stream([H|T]) --> scan_rule(H), token_stream_tail(T).

token_stream_tail(H) --> " ", token_stream(H), !.
token_stream_tail([]) --> [].
