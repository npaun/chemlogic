% excess.pl: Performs calculations involving the stoichiometry of excess quantities.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



combine_structs([],[],[],[]) :- !.
combine_structs([Qty|QtyS],[Coeff|CoeffS],[Formula|FormulaS],[Struct|StructS]) :-
	(Qty = nil -> Struct = nil; Struct = [Qty,Coeff,Formula]),
	combine_structs(QtyS,CoeffS,FormulaS,StructS).

limitation_exists(InputS) :-
	subtract(InputS,[nil],Values),
	length(Values,Count),
	Count > 1.

single_input(InputS,Struct) :-
	subtract(InputS,[nil],[Struct]).

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

stoich_limited(_,_,[],[]) :- !.
stoich_limited(Limitant,LimitantSF,[_|InputS],[nil|QueryS]) :-
	stoich_limited(Limitant,LimitantSF,InputS,QueryS), !.
stoich_limited(Limitant,LimitantSF,[Input|InputS],[[[QtyOut,CalcTypeOut],CoeffOut,FormulaOut]|QueryS]) :-
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
	stoich_limited(Limitant,LimitantSF,InputS,QueryS).

stoich_simple([],[]).
stoich_simple(Input,[nil|QueryS]) :-
	stoich_simple(Input,QueryS).
stoich_simple(Input,[[[QtyOut,_],CoeffOut,FormulaOut]|QueryS]) :-
	Input = [QtyIn,CoeffIn,FormulaIn],
	calc_format(input,FormulaIn,QtyIn,[MolIn,mol],SF), !,
	MolOut is MolIn * CoeffOut / CoeffIn,
	calc_format(output,FormulaOut,[MolOut,mol],QtyOut,SF), !,
	stoich_simple(Input,QueryS).


stoich(InGrammar,Equation,OutGrammar,Balanced,InQtyS,OutQtyS,QueryS) :-
	balance_equation(InGrammar,Equation,OutGrammar,Balanced,CoeffS,FormulaS),
	combine_structs(InQtyS,CoeffS,FormulaS,InputS),
	combine_structs(OutQtyS,CoeffS,FormulaS,QueryS),
	(
		single_input(InputS,Input) -> 
			(
				stoich_simple(Input,QueryS)
			);
			(

				limitant(InputS,InputMolS,Limitant,LimitantSF),
				stoich_limited(Limitant,LimitantSF,InputMolS,QueryS)
			)
	).
