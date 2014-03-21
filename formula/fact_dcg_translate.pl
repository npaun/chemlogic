:- consult('../fact/fact.pl').
:- initialization(cl_parse_all).

:- dynamic [element//2,element_base//2,element_symbol//1]. % The resulting DCG rules for elements.
:- dynamic [group//4,group_base//4,group_symbol//4]. % The resulting DCG rules for groups


/** group_symbol(?Elems,?ElemsR,?Sym) is semidet.

A rule that abstracts away the caching of polyatomic group formulas in different output formats. This rule simply processes the group symbol unformatted, like all of the other rules.

@arg	Elems	The elememnts contained in the polyatomic group	[O,H]
@arg	ElemsR	Difference list tail
@arg	Sym	The internal representation of the group formula	[[O,1],[H,1]]
**/

group_symbol(Elems,ElemsR,Sym) --> group_symbol(user,Elems,ElemsR,Sym).


/** cl_to_dcg(+Clause) is det.

Converts an ordinary element definition (Clause) to grammar rules for its name, base name and symbol.

@arg	Clause	A list of the element symbol, name and base name	(Cl,chlorine,chlor)
*/

cl_to_dcg(Clause) :-
	Clause =.. [_,Sym,Name,Base],

	dcg_translate_rule(element(Sym,Name) --> Name,FullRule),
	assertz(FullRule),

	dcg_translate_rule(element_base(Sym,Base) --> Base,BaseRule),
	assertz(BaseRule),

	dcg_translate_rule(element_symbol(Sym) --> Sym,SymbolRule),
	assertz(SymbolRule).


/** cl_poly_to_dcg(+Clause) is det.

Converts a polyatomic group definition (Clause), to grammar rules for its name, base name and internal formula (Sym). Because of various supported output formats, an additional fact is created for the external formula.

The external formula may simply be a textual representation: NH4
or it could be HTML: NH<sub>4</sub>.

@todo Make group_symbol_output include the output format in use, so that its results can be cached.
@arg	Clause	A list of the group internal formula, name and base name	([[C,1],[N,1]], cyanide, cyan)
*/

cl_poly_to_dcg(Clause) :-
	Clause =.. [_Functor,Sym,Name,Base],
	(formula(user,Elems,[],Sym,[],Formula,[]), !),

	append(Elems,ElemsR,ElemsL),

        dcg_translate_rule(group(ElemsL,ElemsR,Sym,Name) --> Name,FullRule),
        assertz(FullRule),

        dcg_translate_rule(group_base(ElemsL,ElemsR,Sym,Base) --> Base,BaseRule),
        assertz(BaseRule),

	% Because polyatomic groups have formulas, like NH4, we need to store it formatted in various ways. Perhaps we should re-format it on the fly???
	dcg_translate_rule(group_symbol(user,ElemsL,ElemsR,Sym) --> Formula,SymbolRule),
	asserta(SymbolRule),

	(formula(output,Elems,[],Sym,[],OutputFormula,[]), !),

	dcg_translate_rule(group_symbol(output,ElemsL,ElemsR,Sym) --> OutputFormula,OutputSymbolRule),
	asserta(OutputSymbolRule).


cl_oxy_to_dcg(Elem,Charge,OxygenS) :- oxy_to_dcg(Elem,Charge,OxygenS,["per","","","hypo"],["ate","ate","ite","ite"],["ic","ic","ous","ous"]).

oxy_to_dcg(_,_,[],[],[],[]).

oxy_to_dcg(Elem,Charge,[Oxygen|OxygenS],[Prefix|PrefixS],[Suffix|SuffixS],[AcidSuffix|AcidSuffixS]) :-
	(Oxygen > 0 -> 
		(
			oxy_formula(Elem,Charge,Oxygen,Formula),
			oxy_name(Prefix,Elem,Suffix,Name,Base),
			cl_poly_to_dcg(auto(Formula,Name,Base)),
			acid_suffix_to_dcg(Formula,AcidSuffix)
		);
		true),
	oxy_to_dcg(Elem,Charge,OxygenS,PrefixS,SuffixS,AcidSuffixS).

oxy_formula(Elem,Charge,Oxygens,Formula) :-
	Formula = [[Elem,1],["O",Oxygens]],
	assertz(charge(Formula,Charge)).

oxy_name(Prefix,Elem,Suffix,Name,Base) :-
	element_base(Elem,ElemBase,_,[]),
	append(Prefix,ElemBase,Base),
	append(Base,Suffix,Name).


acid_suffix_to_dcg(Formula,Suffix) :-
	dcg_translate_rule(acid_oxyanion_suffix(Formula) --> Suffix,Rule),
	assertz(Rule).

/** cl_parse_all is det.

Causes side-effects. Parses element and polyatomic group facts to create various grammatical rules needed by various parsers. Output is cachable.
*/

cl_parse_all :-
	findall(_,
	(
		cl(Clause),
		cl_to_dcg(Clause)
	),
	_), %? Does not produce a result; causes side-efects instead

	findall(_,
	(
		cl_poly(Clause),
		cl_poly_to_dcg(Clause)
	),
	_),

	findall(_,
	(
		oxyanions(Elem,Charge,Oxygens),
		cl_oxy_to_dcg(Elem,Charge,Oxygens)
	),
	_).
