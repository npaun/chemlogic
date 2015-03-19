simple_map([],[],[]).
simple_map([Filter|FilterS],[List|ListS],Result) :-
	(nonvar(Filter) -> Result = [List|ResultS]; Result = ResultS),
	simple_map(FilterS,ListS,ResultS).

filter_query([],[],[],[],[],[]).

filter_query([Qty|QtyS],[Coeff|CoeffS],[Formula|FormulaS],QtyRes,CoeffRes,FormulaRes) :-
	(
		nonvar(Qty) ->
				(
					QtyRes = [Qty|QtyResS],
					CoeffRes = [Coeff|CoeffResS],
					FormulaRes = [Formula|FormulaResS]
				);
				(
					QtyRes = QtyResS,
					CoeffRes = CoeffResS,
					FormulaRes = FormulaResS
				)
	),
	filter_query(QtyS,CoeffS,FormulaS,QtyResS,CoeffResS,FormulaResS).

limitant([QtyIn|QtyInS],[CoeffIn|CoeffInS],[FormulaIn|FormulaInS],QtyOut,CoeffOut,FormulaOut,[Solution|SolutionS]) :-
	copy_term(QtyOut,Solution),
	stoich(CoeffIn,FormulaIn,QtyIn,CoeffOut,FormulaOut,Solution),
	limitant(QtyInS,CoeffInS,FormulaInS,QtyOut,CoeffOut,FormulaOut,SolutionS).

limiting([],[],[],[],[]).
limiting([QtyIn|QtyInS],[CoeffIn|CoeffInS],[FormulaIn|FormulaInS],[Mol|MolS],[MolRatio|MolRatioS]) :-
	calc_format(input,FormulaIn,QtyIn,[Mol,mol],_),
	MolRatio is Mol / CoeffIn,
	limiting(QtyInS,CoeffInS,FormulaInS,MolS,MolRatioS).


real_limiting(QtyS,CoeffS,FormulaS,MolS,MolLim,CoeffLim,FormulaLim) :-
	limiting(QtyS,CoeffS,FormulaS,MolS,MolRatioS),
	min_list(MolRatioS,MinMolRatio),
	nth0(Index,MolRatioS,MinMolRatio),
	nth0(Index,CoeffS,CoeffLim),
	nth0(Index,FormulaS,FormulaLim),
	nth0(Index,MolS,MolLim).

%compute_excess([Total,Unit],[Actual,Unit],[Excess,Unit]) :-

% vi: ft=prolog
