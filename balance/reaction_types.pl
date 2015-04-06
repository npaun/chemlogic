reaction_match(double_replacement,[
					[
						[[MSym1,_],[NMSym1,_]],
						[[MSym2,_],[NMSym2,_]]
					],
					[
						[[MSym1,_],[NMSym2,_]],
						[[MSym2,_],[NMSym1,_]]
					]
				]).

reaction_complete(double_replacement,_,ElementSideSet,_,[Product1,Product2],[ElementSideSet|_],[[Reactant1,Reactant2],[Product1,Product2]]) :-
	ionic:rev_algo(Reactant1,[MSym1,MCharge1,NMSym1,NMCharge1]),
	ionic:rev_algo(Reactant2,[MSym2,MCharge2,NMSym2,NMCharge2]),
	ionic:fwd_algo(Product1,[MSym1,MCharge1,NMSym2,NMCharge2]),
	ionic:fwd_algo(Product2,[MSym2,MCharge2,NMSym1,NMCharge1]).


reaction_info(neutralization,'Neutralization').
reaction_info(double_replacement,'Double Replacement').
reaction_info(single_replacement,'Single Replacmenet').
reaction_info(combustion,'Combustion of Hydrocarbons').
reaction_info(decomposition,'Decomposition').
reaction_info(synthesis,'Synthesis').

% vi: ft=prolog
