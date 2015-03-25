% excess.pl: Performs calculations involving the stoichiometry of excess quantities.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



filter_query([],[],[],[]) :- !.
filter_query([Qty|QtyS],[Coeff|CoeffS],[Formula|FormulaS],[Struct|StructS]) :-
	(Qty = nil -> Struct = nil; Struct = [Qty,Coeff,Formula]),
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

limit_calculate(_,_,[]) :- !.
limit_calculate(Limitant,LimitantSF,[nil|QueryS]) :-
	limit_calculate(Limitant,LimitantSF,QueryS).
limit_calculate(Limitant,LimitantSF,[[QtyOut,CoeffOut,FormulaOut]|QueryS]) :-
	Limitant = [[MolIn,mol],CoeffIn,_],
	MolOut is MolIn * CoeffOut / CoeffIn,
	calc_format(output,FormulaOut,[MolOut,mol],QtyOut,LimitantSF), !,
	limit_calculate(Limitant,LimitantSF,QueryS).

limit_calculate(_,_,[],[]) :- !.
limit_calculate(Limitant,LimitantSF,[_|InputS],[nil|QueryS]) :-
	limit_calculate(Limitant,LimitantSF,InputS,QueryS), !.
limit_calculate(Limitant,LimitantSF,[Input|InputS],[[[QtyOut,CalcTypeOut],CoeffOut,FormulaOut]|QueryS]) :-
	Limitant = [[MolLim,mol],CoeffLim,_],
	(
		CalcTypeOut = excess ->
			(
				Input = [[MolIn,mol],_,_], 
				MolOut is MolIn - MolLim * CoeffOut / CoeffLim
			);
			MolOut is MolLim * CoeffOut / CoeffLim
	),
	calc_format(output,FormulaOut,[MolOut,mol],QtyOut,LimitantSF), !,
	limit_calculate(Limitant,LimitantSF,InputS,QueryS).

stoich_excess(InGrammar,Equation,OutGrammar,Balanced,InQtyS,OutQtyS,QueryS) :-
	balance_equation(InGrammar,Equation,OutGrammar,Balanced,CoeffS,FormulaS),
	filter_query(InQtyS,CoeffS,FormulaS,InputS),
	limitant(InputS,InputMolS,Limitant,LimitantSF),
	filter_query(OutQtyS,CoeffS,FormulaS,QueryS),
	limit_calculate(Limitant,LimitantSF,InputMolS,QueryS).
