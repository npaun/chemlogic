%:- module(cl_error,[parse/3,syntax_stop/3,syntax_stop/4]).

scan_rule(alpha,[C|T]) --> [C], scan_rule_r(alpha,T).


scan_rule(white,C) --> scan_rule(punct,C).

scan_rule(punct,[C|T]) --> 
	        [C],
		        {\+ char_type(C,alnum)}, !,
			        scan_rule(punct,T).


scan_rule(digit,[C|T]) -->
	[C],
	{\+ char_type(C,alpha)}, !,
	scan_rule(digit,T).


scan_rule(inside_paren,[C|T]) -->
	[C],
	{\+ C = ')'}, !,
	scan_rule(inside_paren,T).


scan_rule(group,[C|T]) -->
	[C],
	{\+ (char_type(C,punct); char_type(C,white))}, !,
	scan_rule(group,T).

scan_rule(_,[]) -->
	[].



punct_check, [T] --> [T], {char_type(T,punct)}.



scan_rule_r(alpha,[C|T]) -->
	[C],
	({char_type(C,white)} -> \+ punct_check; {true}),
	{\+ (char_type(C,digit); char_type(C,upper); char_type(C,punct); char_type(C,white))}, !,
	scan_rule_r(alpha,T).

scan_rule_r(_,[]) --> [].


highlight_error(Start,Token,Rest) :-
	writef('%s\e[01;41;37m%s\e[00m%s\n',[Start,Token,Rest]).

find_token([],[],nil,[]).

find_token(Unparsed,Token,Type,Rest) :-
	Unparsed = [First|_],
	(
	char_type(First,alpha) -> Type = alpha;
	char_type(First,punct) -> Type = punct;
	char_type(First,digit) -> Type = digit;
	char_type(First,white) -> Type = white
	), !,

	scan_rule(Type,Token,Unparsed,Rest).


find_token_special(Type,Unparsed,Token,Type,Rest) :-
	scan_rule(Type,Token,Unparsed,Rest).

parse_error_debug(Unparsed,Token,ErrCode,TokenType) :- write('Scan '), writeln(Unparsed), write(ErrCode), write(': '), write(Token), write(' is '), writeln(TokenType).


explain_error(Input,Error,Flags,Unparsed) :-
	(Flags = [] -> 
		find_token(Unparsed,Token,TokenType,Rest); 
		find_token_special(Flags,Unparsed,Token,TokenType,Rest)
	),

	append(Start,Unparsed,Input), !,
	parse_error_debug(Unparsed,Token,Error,TokenType),
	highlight_error(Start,Token,Rest),

	(Error = Module:ErrCode),

	(Module:guidance_errcode(ErrCode,TokenType,MessageErrCode) -> writeln(MessageErrCode); true),
	nl,
	(Module:guidance_unparsed(Unparsed,MessageUnparsed) -> (write(MessageUnparsed), writeln(ErrCode)); true),
	!,fail.


phrase_fluff_check(Clause,Input,Output) :-
	((phrase(Clause,Input,Rest), !,
	(
	Output = Rest, !;
	syntax_stop(none,Rest,[])
	)); syntax_stop(fail,Input,[])).

:- meta_predicate parse(//,?,?).

parse(Clause,Input,Output) :-
	catch(
	phrase_fluff_check(Clause,Input,Output),
	error(syntax_error(ErrCode,Flags),Unparsed),
	explain_error(Input,ErrCode,Flags,Unparsed)
	). 

syntax_stop(Expected,Unprocessed,_) :- syntax_stop(Expected,[],Unprocessed,_).

syntax_stop(Expected,Flags,Unprocessed,_) :-
	throw(error(syntax_error(Expected,Flags),Unprocessed)).

% vi: filetype=prolog
