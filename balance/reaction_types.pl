reaction_match(double_replacement,[
					[
						[[MSym1,MSub1],[NMSym1,NMSub1]],
						[[MSym2,MSub2],[NMSym2,NMSub2]]
					],
					[
						[[MSym1,MSubP1],[NMSym2,NMSubP2]],
						[[MSym2,MSubP2],[NMSym1,NMSubP1]]
					]
				]).

reaction_complete(double_replacement,_,ElementSideSet,_,[Product1,Product2],[ElementSideSet|_],[[Reactant1,Reactant2],[Product1,Product2]]) :-
	ionic:rev_algo(Reactant1,[MSym1,MCharge1,NMSym1,NMCharge1]),
	ionic:rev_algo(Reactant2,[MSym2,MCharge2,NMSym2,NMCharge2]),
	ionic:fwd_algo(Product1,[MSym1,MCharge1,NMSym2,NMCharge2]),
	ionic:fwd_algo(Product2,[MSym2,MCharge2,NMSym1,NMCharge1]).



% vi: ft=prolog
