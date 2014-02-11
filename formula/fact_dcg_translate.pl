:- consult('../fact/fact.pl').
:- initialization(cl_parse_all).

:- dynamic [element//2,element_base//2,element_symbol//1]. % The resulting DCG rules for elements.
:- dynamic [group//4,group_base//4,group_symbol//3,group_symbol_output//3]. % The resulting DCG rules for groups


cl_to_dcg(Clause) :-
	Clause =.. [_,Sym,Name,Base],

	dcg_translate_rule(element(Sym,Name) --> Name,FullRule),
	assertz(FullRule),

	dcg_translate_rule(element_base(Sym,Base) --> Base,BaseRule),
	assertz(BaseRule),

	dcg_translate_rule(element_symbol(Sym) --> Sym,SymbolRule),
	assertz(SymbolRule).


cl_poly_to_dcg(Clause) :-
	Clause =.. [_Functor,Sym,Name,Base],
	(formula(user,Contents,[],Sym,Formula,[]), !),

	append(Contents,ElemsR,Elems),

        dcg_translate_rule(group(Elems,ElemsR,Sym,Name) --> Name,FullRule),
        assertz(FullRule),

        dcg_translate_rule(group_base(Elems,ElemsR,Sym,Base) --> Base,BaseRule),
        assertz(BaseRule),

	dcg_translate_rule(group_symbol(Elems,ElemsR,Sym) --> Formula,SymbolRule),
	asserta(SymbolRule),

	(formula(output,Contents,[],Sym,OutputFormula,[]), !),

	dcg_translate_rule(group_symbol_output(Elems,ElemsR,Sym) --> OutputFormula,OutputSymbolRule),
	asserta(OutputSymbolRule).

cl_parse_all :-
	findall(_,
	(
	cl(X),
	cl_to_dcg(X)
	),
	_), %? Does not produce a result; causes side-efects instead
	findall(_,
	(
	cl_poly(X),
	cl_poly_to_dcg(X)
	),
	_).


