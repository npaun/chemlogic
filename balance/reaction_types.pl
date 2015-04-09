
%%% Neutralization Reactions %%%
reaction_match(neutralization,[
					[[["H",_],[NMSym1,_]],[[MSym2,_],[[["O", 1], ["H", 1]], 1]]],
					[[[MSym2,_],[NMSym1,_]],[["H", 2], ["O", 1]]]
				],certain).

reaction_match(neutralization,[
					[[["H",_],[NMSym1,_]],[[MSym2,_],[[["O", 1], ["H", 1]], 1]]],
					[[["H", 2], ["O", 1]],[[MSym2,_],[NMSym1,_]]]
				],certain).

reaction_match(neutralization,[
					[[[MSym2,_],[[["O", 1], ["H", 1]], 1]],[["H",_],[NMSym1,_]]],
					[[[MSym2,_],[NMSym1,_]],[["H", 2], ["O", 1]]]
				],certain).

reaction_match(neutralization,[
					[[[MSym2,_],[[["O", 1], ["H", 1]], 1]],[["H",_],[NMSym1,_]]],
					[[["H", 2], ["O", 1]],[[MSym2,_],[NMSym1,_]]]
				],certain).


%%% Double Replacement Reactions %%%
reaction_match(double_replacement,[
					[[[MSym1,_],[NMSym1,_]],[[MSym2,_],[NMSym2,_]]],
					[[[MSym1,_],[NMSym2,_]],[[MSym2,_],[NMSym1,_]]]
				],Reacts) :-
				activity_check_multiple(MSym1,MSym2,NMSym1,NMSym2,Reacts).

reaction_match(double_replacement,[
					[[[MSym1,_],[NMSym1,_]],[[MSym2,_],[NMSym2,_]]],
					[[[MSym2,_],[NMSym1,_]],[[MSym1,_],[NMSym2,_]]]
				],Reacts) :-
				activity_check_multiple(MSym1,MSym2,NMSym1,NMSym2,Reacts).

%%% Single Replacement Reactions %%%
reaction_match(single_replacement,[
					[[[MSym1,_],[NMSym1,_]],[[MSym2,_]]],
					[[[MSym2,_],[NMSym1,_]],[[MSym1,_]]]
				],Reacts) :-
				activity_check(MSym1,MSym2,Reacts).

reaction_match(single_replacement,[
					[[[MSym1,_],[NMSym1,_]],[[NMSym2,_]]],
					[[[MSym1,_],[NMSym2,_]],[[NMSym1,_]]]
				], Reacts) :-
				activity_check(NMSym1,NMSym2,Reacts).


%%% Synthesis and Decomposition Reactions %%%

reaction_match(synthesis,[
				_,
				[_]
			],unknown).

reaction_match(decomposition,[
					[_],
					_
			],unknown).

%%% Complete chemical reactions %%%
reaction_complete(neutralization,_,ElementSideSet,_,[Product1,Water],[ElementSideSet|_],[[Reactant1,Reactant2],[Product1,Water]]) :-
	Water = [["H",2],["O",1]],
	ionic:rev_algo(Reactant1,["H",_,NMSym1,NMCharge1]),
	ionic:rev_algo(Reactant2,[MSym2,MCharge2,[["O",1],["H",1]],_]),
	ionic:fwd_algo(Product1,[MSym2,MCharge2,NMSym1,NMCharge1]).

reaction_complete(double_replacement,_,ElementSideSet,_,[Product1,Product2],[ElementSideSet|_],[[Reactant1,Reactant2],[Product1,Product2]]) :-
	ionic:rev_algo(Reactant1,[MSym1,MCharge1,NMSym1,NMCharge1]),
	ionic:rev_algo(Reactant2,[MSym2,MCharge2,NMSym2,NMCharge2]),
	MSym1 \= MSym2,
	NMSym1 \= NMSym2,
	ionic:fwd_algo(Product1,[MSym1,MCharge1,NMSym2,NMCharge2]),
	ionic:fwd_algo(Product2,[MSym2,MCharge2,NMSym1,NMCharge1]).

reaction_complete(single_replacement,_,ElementSideSet,_,[Product1,Product2],[ElementSideSet|_],[[Reactant1,Reactant2],[Product1,Product2]]) :-
	ionic:rev_algo(Reactant1,[MSym1,_,NMSym1,NMCharge1]),

	(Reactant2 = [[MSym2,_]], charge_check(metal,MSym2,MChargeS),
		(
			is_list(MChargeS) -> MChargeS = [MCharge2|_]; % If the free metal is multivalent, we will just guess and pick the first charge listed (hopefully most common)
			MChargeS = MCharge2
		)
	),
	
	ionic:fwd_algo(Product1,[MSym2,MCharge2,NMSym1,NMCharge1]),
	name:pure_process([MSym1],[],Product2,_,[]).

reaction_complete(single_replacement,_,ElementSideSet,_,[Product1,Product2],[ElementSideSet|_],[[Reactant1,Reactant2],[Product1,Product2]]) :-
	ionic:rev_algo(Reactant1,[MSym1,MCharge1,NMSym1,_]),

	(Reactant2 = [[NMSym2,_]], charge(NMSym2,NMCharge2)),
	ionic:fwd_algo(Product1,[MSym1,MCharge1,NMSym2,NMCharge2]),
	name:pure_process([NMSym1],[],Product2,_,[]).


%%% Information about reactions %%%
reaction_info(neutralization,'Neutralization').
reaction_info(double_replacement,'Double Replacement').
reaction_info(single_replacement,'Single Replacement').
reaction_info(combustion,'Combustion of Hydrocarbons').
reaction_info(decomposition,'Decomposition').
reaction_info(synthesis,'Synthesis').

%%% Check if the reaction will occur, using the activity series.

activity_check(Elem1,Elem2,Reacts) :-
		activity(Elem1,ElemAct1),
		activity(Elem2,ElemAct2),
		(ElemAct2 < ElemAct1 -> Reacts = yes; Reacts = no), !.

activity_check(_,_,unknown).

activity_check_multiple(Elem1,Elem2,Elem3,Elem4,Reacts) :-
	activity_check(Elem1,Elem2,Reacts),
	activity_check(Elem3,Elem4,Reacts).

activity_check_multiple(_,_,_,_,multiple).

%%% Information about activity guesses %%%
activity_info(certain,'Yes').
activity_info(yes,'Probably').
activity_info(no,'Probably not').

% vi: ft=prolog
