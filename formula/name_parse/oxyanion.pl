

oxyanion([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> per_ate(Sym,Oxygens,Charge).
oxyanion([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> ate(Sym,Oxygens,Charge).
oxyanion([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> ite(Sym,Oxygens,Charge).
oxyanion([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> hypo_ite(Sym,Oxygens,Charge).



oxyanion_acid([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> per_ic(Sym,Oxygens,Charge).
oxyanion_acid([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> ic(Sym,Oxygens,Charge).
oxyanion_acid([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> ous(Sym,Oxygens,Charge).
oxyanion_acid([Sym,"O"|Rest],Rest,[[Sym,1],["O",Oxygens]],Charge) --> hypo_ous(Sym,Oxygens,Charge).



per_ate(Sym,Oxygens,Charge) --> "per", element_base(Sym,_), "ate",
	{
	oxyanions(Sym,Charge,[Oxygens,_,_,_]),
	Oxygens > 0
	}.


per_ic(Sym,Oxygens,Charge) --> "per", acid_base(Sym), "ic",
	{
	oxyanions(Sym,Charge,[Oxygens,_,_,_]),
	Oxygens > 0
	}.

ate(Sym,Oxygens,Charge) --> element_base(Sym,_), "ate",
	{
	oxyanions(Sym,Charge,[_,Oxygens,_,_]),
	Oxygens > 0
	}.


ic(Sym,Oxygens,Charge) --> acid_base(Sym), "ic",
	{
	oxyanions(Sym,Charge,[_,Oxygens,_,_]),
	Oxygens > 0
	}.


ite(Sym,Oxygens,Charge) --> element_base(Sym,_), "ite",
	{
	oxyanions(Sym,Charge,[_,_,Oxygens,_]),
	Oxygens > 0
	}.


ous(Sym,Oxygens,Charge) --> acid_base(Sym), "ous",
	{
	oxyanions(Sym,Charge,[_,_,Oxygens,_]),
	Oxygens > 0
	}.

hypo_ite(Sym,Oxygens,Charge) --> "hypo", element_base(Sym,_), "ite",
	{
	oxyanions(Sym,Charge,[_,_,_,Oxygens]),
	Oxygens > 0
	}.


hypo_ous(Sym,Oxygens,Charge) --> "hypo", acid_base(Sym), "ous",
	{
	oxyanions(Sym,Charge,[_,_,_,Oxygens]),
	Oxygens > 0
	}.
