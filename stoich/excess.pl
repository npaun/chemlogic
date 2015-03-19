filter_query([],[],[],[]).

filter_query([Qty|QtyS],[Coeff|CoeffS],[Formula|FormulaS],[Struct|StructS]) :-
	(
		nonvar(Qty) ->
				(
					Struct = [[Qty,Coeff,Formula]|StructS]
				);
				(
					Struct = StructS
				)
	),
	filter_query(QtyS,CoeffS,FormulaS,StructS).

mol_ratio([],[],[]).
mol_ratio([[QtyIn,CoeffIn,FormulaIn]|StructS],[MolRatio|MolRatioS],[[[Mol,mol],CoeffIn,FormulaIn]|NewStructS]) :-
	calc_format(input,FormulaIn,QtyIn,[Mol,mol],_),
	MolRatio is Mol / CoeffIn,
	mol_ratio(StructS,MolRatioS,NewStructS).

limitant(StructS,NewStructS,LimitantStruct) :-
	mol_ratio(StructS,MolRatioS,NewStructS),
	min_list(MolRatioS,MinMolRatio),
	nth0(Index,MolRatioS,MinMolRatio),
	nth0(Index,NewStructS,LimitantStruct).

stoich_ui_excess(InGrammar,Equation,OutGrammar,Balanced,QtyS,ResultS) :-
	balance_equation(InGrammar,Equation,OutGrammar,Balanced,CoeffS,FormulaS),
	filter_query(QtyS,CoeffS,FormulaS,InStructS),
	filter_query(ResultS,CoeffS,FormulaS,OutStructS),
	limitant(InStructS,MolStructS,Limitant),
	stoich_excess_calculate(MolStructS,Limitant,OutStructS).

stoich_excess_calculate(_,[],[]).
stoich_excess_calculate(_,Struct,[[QtyOut,CoeffOut,FormulaOut]|OutStructS]) :-
	Struct = [[MolLim,mol],CoeffLim,_],
	MolOut is MolLim * CoeffOut/CoeffLim,
	calc_format(output,FormulaOut,[MolOut,mol],QtyOut,3),
	stoich_excess_calculate(_,Struct,OutStructS).
	


%compute_excess([Total,Unit],[Actual,Unit],[Excess,Unit]) :-

% vi: ft=prolog
