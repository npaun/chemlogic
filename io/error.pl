scan_rule(alpha,[C|T]) -->
	[C],
	({char_type(C,white)} -> \+ punct_check; {true}),
	{\+ char_type(C,digit)}, !,
	scan_rule(alpha,T).

scan_rule(digit,[C|T]) -->
	[C],
	{\+ char_type(C,alpha)}, !,
	scan_rule(digit,T).

scan_rule(_,[]) -->
	[].

punct_check, [T] --> [T], {char_type(T,punct)}.


highlight_error(Start,Token,Rest) :-
	writef('%s\e[01;41;37m%s\e[00m%s\n',[Start,Token,Rest]).

find_token(Unparsed,Token,Type,Rest) :-
	Unparsed = [First|_],
	(
	char_type(First,alpha) -> Type = alpha;
	char_type(First,digit) -> Type = digit;
	char_type(First,white) -> Type = digit
	), !,

	scan_rule(Type,Token,Unparsed,Rest).


parse_error_debug(Token,ErrCode,TokenType) :- write(ErrCode), write(': '), write(Token), write(' is '), writeln(TokenType).

explain_error(Input,ErrCode,Unparsed) :-
	find_token(Unparsed,Token,TokenType,Rest),
	append(Start,Unparsed,Input), !,
	parse_error_debug(Token,ErrCode,TokenType),
	highlight_error(Start,Token,Rest),

	(guidance_errcode(ErrCode,TokenType,MessageErrCode) -> writeln(MessageErrCode); true),
	(guidance_unparsed(Unparsed,MessageUnparsed) -> (write(MessageUnparsed), writeln(ErrCode)); true).


phrase_fluff_check(Clause,Input,Output) :-
	phrase(Clause,Input,Rest), !,
	(
	Output = Rest, !;
	syntax_stop(none,Rest,[])
	).

parse(Clause,Input,Output) :-
	catch(
	phrase_fluff_check(Clause,Input,Output),
	error(syntax_error(ErrCode),Unparsed),
	explain_error(Input,ErrCode,Unparsed)
	).

syntax_stop(Expected,Unprocessed,_) :-
	throw(error(syntax_error(Expected),Unprocessed)).
% vi: filetype=prolog