:- include('../fact/fact.pl').
:- initialization(cl_parse_all).

:- dynamic [element//2,element_base//2,element_symbol//1]. % The resulting DCG rules for elements.
:- dynamic [group//4,group_base//4,group_symbol//3,group_symbol_output//3]. % The resulting DCG rules for groups


cl_to_dcg(Clause) :-
	Clause =.. [_,Symbol,Name,Base],
	dcg_translate_rule(element(Symbol,Name) --> Name,FullRule),
	assertz(FullRule),
	dcg_translate_rule(element_base(Symbol,Base) --> Base,BaseRule),
	assertz(BaseRule),
	% (formula(Symbol,Formula,[]), !),
	dcg_translate_rule(element_symbol(Symbol) --> Symbol,SymbolRule),
	assertz(SymbolRule).


cl_poly_to_dcg(Clause) :-
	Clause =.. [_,Symbol,Name,Base],
	(formula(user,Contents,[],Symbol,Formula,[]), !),

	append(Contents,Rest,Comb),

        dcg_translate_rule(group(Comb,Rest,Symbol,Name) --> Name,FullRule),
        assertz(FullRule),

        dcg_translate_rule(group_base(Comb,Rest,Symbol,Base) --> Base,BaseRule),
        assertz(BaseRule),

	dcg_translate_rule(group_symbol(Comb,Rest,Symbol) --> Formula,SymbolRule),
	asserta(SymbolRule),
	
	(formula(output,Contents,[],Symbol,OutputFormula,[]), !),
	
	dcg_translate_rule(group_symbol_output(Comb,Rest,Symbol) --> OutputFormula,OutputSymbolRule),
	asserta(OutputSymbolRule).

cl_parse_all :-
	findall(_,
		(
		cl(X),
		cl_to_dcg(X)
		),
	_),
	findall(_,
		(
		 cl_poly(X),
		 cl_poly_to_dcg(X)
		),
	_).


