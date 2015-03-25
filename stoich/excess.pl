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
mol_ratio([nil|StructS],[nil|MolRatioS],[nil|NewStructS]) :-
	mol_ratio(StructS,MolRatioS,NewStructS).
mol_ratio([[QtyIn,CoeffIn,FormulaIn]|StructS],[MolRatio|MolRatioS],[[[Mol,mol],SF,CoeffIn,FormulaIn]|NewStructS]) :-
	calc_format(input,FormulaIn,QtyIn,[Mol,mol],SF),
	MolRatio is Mol / CoeffIn,
	mol_ratio(StructS,MolRatioS,NewStructS).

limitant(StructS,NewStructS,LimitantStruct) :-
	mol_ratio(StructS,MolRatioS,NewStructS),
	min_member(MinMolRatio,MolRatioS),
	nth0(Index,MolRatioS,MinMolRatio), !,
	nth0(Index,NewStructS,LimitantStruct).

stoich_limited(_,[],[]) :- !.
stoich_limited(Limitant,[_|InputS],[nil|QueryS]) :-
	stoich_limited(Limitant,InputS,QueryS), !.
stoich_limited(Limitant,[Input|InputS],[[[QtyOut,CalcTypeOut],CoeffOut,FormulaOut]|QueryS]) :-
	Limitant = [[MolLim,mol],SFLim,CoeffLim,_],
	(
		CalcTypeOut = excess ->
			(
				Input = [[MolIn,mol],SFIn,_,_], 
				SF is min(SFLim,SFIn),
				MolOut is MolIn - MolLim * CoeffOut / CoeffLim
			);
			(
				MolOut is MolLim * CoeffOut / CoeffLim,
				SF = SFLim
			)
	),
	calc_format(output,FormulaOut,[MolOut,mol],QtyOut,SF), !,
	stoich_limited(Limitant,InputS,QueryS).

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

				limitant(InputS,InputMolS,Limitant),
				stoich_limited(Limitant,InputMolS,QueryS)
			)
	).
