:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes,chars).

atomics_to_chars([],[]).

atomics_to_chars([Atom|AtomS],[Chars|CharsS]) :-
	atom_chars(Atom,Chars),
	atomics_to_chars(AtomS,CharsS).

tokenizer(In,Out) :-
	atomic_list_concat(AtomsRaw,' ',In),
	delete(AtomsRaw,'',Atoms),
	atomics_to_chars(Atoms,Out).
