oxyanion_test(Sym,List,Oxygens,Charge) --> 
	({oxyanions(Sym,Charge,List)} ; syntax_stop(no_oxyanions)), 
	({Oxygens > 0}; syntax_stop(invalid_oxyanion)).

oxyanion([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> oxyanion_name(Sym,Oxygens,Charge).

oxyanion_name(Sym,Oxygens,Charge) --> "per", element_base(Sym,_), "ate",
		oxyanion_test(Sym,[Oxygens,_,_,_],Oxygens,Charge).

oxyanion_name(Sym,Oxygens,Charge) --> element_base(Sym,_), "ate",
		oxyanion_test(Sym,[_,Oxygens,_,_],Oxygens,Charge).

oxyanion_name(Sym,Oxygens,Charge) --> element_base(Sym,_), "ite",
		oxyanion_test(Sym,[_,_,Oxygens,_],Oxygens,Charge).

oxyanion_name(Sym,Oxygens,Charge) --> "hypo", element_base(Sym,_), "ite",
		oxyanion_test(Sym,[_,_,_,Oxygens],Oxygens,Charge).


oxyanion_acid([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> oxyanion_acid_name(Sym,Oxygens,Charge).

acid_base(Sym) --> element_base(Sym,_), 
	(acid_ion_suffix(Sym) -> {true} ; syntax_stop(acid_suffix)).

oxyanion_acid_name(Sym,Oxygens,Charge) --> "per",  acid_base(Sym), "ic",
		oxyanion_test(Sym,[Oxygens,_,_,_],Oxygens,Charge).

oxyanion_acid_name(Sym,Oxygens,Charge) --> acid_base(Sym) , "ic",
		oxyanion_test(Sym,[_,Oxygens,_,_],Oxygens,Charge).


oxyanion_acid_name(Sym,Oxygens,Charge) --> acid_base(Sym), "ous",
		oxyanion_test(Sym,[_,_,Oxygens,_],Oxygens,Charge).

oxyanion_acid_name(Sym,Oxygens,Charge) --> "hypo", acid_base(Sym), "ous",
		oxyanion_test(Sym,[_,_,_,Oxygens],Oxygens,Charge).
