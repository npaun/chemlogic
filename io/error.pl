% error.pl: Handles syntax errors, identifies error tokens and rethrows errors for handling with additional context
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



scan_rule(alpha,[C|T]) --> [C], scan_rule_r(alpha,T).

scan_rule(white,C) --> scan_rule(punct,C).

scan_rule(punct,[C|T]) -->
	        [C],
		{\+ char_type(C,alnum)}, !,
		scan_rule(punct,T).


scan_rule(digit,[C|T]) -->
	[C],
	(
		{char_type(C,white)} -> \+ punct_check;
       		{true}
	),
	{\+ char_type(C,alpha); char_type(C,punct); char_type(C,white)}, !,
	scan_rule(digit,T).


scan_rule(inside_paren,[C|T]) -->
	[C],
	{\+ C = ')'}, !,
	scan_rule(inside_paren,T).

scan_rule(outside_paren,[C|T]) -->
	[C],!,
	(
		{ C = ')'}, ! -> {T = []};
	       	scan_rule(outside_paren,T)
	).

scan_rule(group,[C|T]) -->
	[C],
	{\+ (char_type(C,punct); char_type(C,white))}, !,
	scan_rule(group,T).

scan_rule(mark,[C]) --> [C].

scan_rule(_,[]) -->
	[].

punct_check, [T] --> [T], {char_type(T,punct)}.

scan_rule_r(alpha,[C|T]) -->
	[C],
	({char_type(C,white)} -> \+ punct_check; {true}),
	{\+ (char_type(C,digit); char_type(C,upper); char_type(C,punct); char_type(C,white))}, !,
	scan_rule_r(alpha,T).

scan_rule_r(_,[]) --> [].

scan_rule_last([C]) --> [C].



find_token([],[],nil,[]).

find_token(Unparsed,Token,Type,Rest) :-
	Unparsed = [First|_],
	(
	First = '(' -> Type = outside_paren;
	char_type(First,alpha) -> Type = alpha;
	char_type(First,punct) -> Type = punct;
	char_type(First,digit) -> Type = digit;
	char_type(First,white) -> Type = white
	), !,

	scan_rule(Type,Token,Unparsed,Rest).


find_token_special(Type,Unparsed,Token,Type,Rest) :-
	scan_rule(Type,Token,Unparsed,Rest).

explain_syntax_error(ParseModule,Input,Error,Flags,Unparsed,InfoStruct) :-
	(Flags = [] ->
		find_token(Unparsed,Token,TokenType,Rest);
		find_token_special(Flags,Unparsed,Token,TokenType,Rest)
	),

	append(Start,Unparsed,Input), !,
	HighlightStruct = highlight(Start,Token,Rest),

	debug(chemlogic_custom,"Scan ~w~n~w: ~w is ~w",[Unparsed,Error,Token,TokenType]),

	(Error = Module:ErrCode; (ParseModule = Module, Error = ErrCode)),

	(Module:guidance_errcode(ErrCode,TokenType,MessageErrCode) ; MessageErrCode = ''),
	(Module:guidance_unparsed(Unparsed,MessageUnparsed); MessageUnparsed = 'Error Code: '),
	!,
	MessageStruct = message(MessageErrCode,MessageUnparsed,ErrCode),
	InfoStruct = [HighlightStruct,MessageStruct].


explain_general_error(Input,Error,DebugData,InfoStruct) :-
	HighlightStruct = highlight([],[],Input),

	(Error = Module:Type),

	(Module:explain_general_data(DebugData,DebugInfo); DebugInfo = DebugData),
	!,
	(Module:guidance_general(Type,MessageType); MessageType = ''),
	MessageStruct = message(MessageType,DebugInfo),

	InfoStruct = [HighlightStruct,MessageStruct].

explain_general_rethrow(Type,Input,Error,DebugData) :-
	explain_general_error(Input,Error,DebugData,InfoStruct),
	ErrorTerm =.. [Type,InfoStruct],
	throw(error(ErrorTerm,_)).

explain_syntax_rethrow(Module,Input,ErrCode,Flags,Unparsed) :-
	explain_syntax_error(Module,Input,ErrCode,Flags,Unparsed,InfoStruct),
	throw(error(syntax_error(InfoStruct),_)).


phrase_fluff_check(Clause,Input,Output) :-
	((phrase(Clause,Input,Rest), !,
	(
	Output = Rest, !;
	syntax_stop(none,Rest,[])
	)); syntax_stop(fail,Input,[])).

:- meta_predicate parse(?,?,?).

parse(Call,Input) :- parse(Call,Input,[]).

parse(Call,Args,Input,Output) :-
	Args =.. [_|T],
	Clause =.. [Call|T],
	parse(Clause,Input,Output).

parse(Clause,Input,Output) :-
	functor(Clause,Module,_),
	catch(
	catch(
		phrase_fluff_check(Clause,Input,Output),
		error(syntax_error(ErrCode,Flags),Context),
		explain_syntax_rethrow(Module,Input,ErrCode,Flags,Context)
		),
		error(logic_error(LogicErrCode,LogicInfo),_),
		explain_general_rethrow(logic_error,Input,LogicErrCode,LogicInfo)
	).


process(Clause) :-
	functor(Clause,Module,_),
	catch(
	catch(
		call(Clause),
		error(syntax_error(ErrCode,Flags),Context),
		explain_syntax_rethrow(Module,[],ErrCode,Flags,Context)
		),
		error(logic_error(LogicErrCode,LogicInfo),_),
		explain_general_rethrow(logic_error,[],LogicErrCode,LogicInfo)
	).

syntax_stop(Expected,Unprocessed,_) :- syntax_stop(Expected,[],Unprocessed,_).

syntax_stop(Expected,Flags,Unprocessed,_) :-
	throw(error(syntax_error(Expected,Flags),Unprocessed)).

% vi: filetype=prolog
