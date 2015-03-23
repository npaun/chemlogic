filter_query([],[],[],[]) :- !.

filter_query([Qty|QtyS],[Coeff|CoeffS],[Formula|FormulaS],[Struct|StructS]) :-
	(
		Qty = nil ->
				(

					Struct = nil
				);
				(

					Struct = [Qty,Coeff,Formula]
				)
	),
	filter_query(QtyS,CoeffS,FormulaS,StructS).

mol_ratio([],[],[]).
mol_ratio([nil|StructS],[[nil,0]|MolRatioS],[nil|NewStructS]) :-
	mol_ratio(StructS,MolRatioS,NewStructS).

mol_ratio([[QtyIn,CoeffIn,FormulaIn]|StructS],[[MolRatio,SF]|MolRatioS],[[[Mol,mol],CoeffIn,FormulaIn]|NewStructS]) :-
	calc_format(input,FormulaIn,QtyIn,[Mol,mol],SF),
	MolRatio is Mol / CoeffIn,
	mol_ratio(StructS,MolRatioS,NewStructS).

limitant(StructS,NewStructS,LimitantStruct,LimitantSF) :-
	mol_ratio(StructS,MolRatioS,NewStructS),
	min_member(MinMolRatio,MolRatioS),
	MinMolRatio = [_,LimitantSF],
	nth0(Index,MolRatioS,MinMolRatio), !,
	nth0(Index,NewStructS,LimitantStruct).

excess_calculate(_,_,[]) :- !.
excess_calculate(Limitant,LimitantSF,[[QtyOut,CoeffOut,FormulaOut]|QueryS]) :-
	Limitant = [[MolIn,mol],CoeffIn,_],
	MolOut is MolIn * CoeffOut / CoeffIn,
	calc_format(output,FormulaOut,[MolOut,mol],QtyOut,LimitantSF), !,
	excess_calculate(Limitant,LimitantSF,QueryS).

% vi: ft=prolog
