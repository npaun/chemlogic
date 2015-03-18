% units.pl: Defines the conversion and formatting of units of various chemical quantities.
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



:- consult('../balance/balancer').
:- use_module(sigfigs).



stoich(CoeffK,FormulaK,QtyK,CoeffR,FormulaR,QtyR) :-
	calc_format(input,FormulaK,QtyK,[MolK,mol],SF), !,
	MolR is MolK * CoeffR / CoeffK,
	calc_format(output,FormulaR,[MolR,mol],QtyR,SF), !.

		
calc_format(input,Formula,[QtyIn,UnitIn],[QtyOut,UnitOut],SF) :-
	sigfigs(QtyIn,SF),
	to_number(QtyIn,QtyInNum),
	unit(Formula,[QtyInNum,UnitIn],[QtyOut,UnitOut]).

calc_format(output,Formula,[QtyIn,UnitIn],[QtyOutRound,UnitOut],SF) :-
	unit(Formula,[QtyIn,UnitIn],[QtyOut,UnitOut]),
	round_sigfigs(QtyOut,SF,QtyOutRound).


calc_format(input,Formula,[[QtyIn1,UnitIn1],[QtyIn2,UnitIn2]],[QtyOut,UnitOut],SF) :-
	sigfigs(QtyIn1,SF1),
	to_number(QtyIn1,QtyInNum1),
	sigfigs(QtyIn2,SF2),
	to_number(QtyIn2,QtyInNum2),
	SF is min(SF1,SF2),
	unit(Formula,[[QtyInNum1,UnitIn1],[QtyInNum2,UnitIn2]],[QtyOut,UnitOut]).

% Perhaps output from the 2 unit form might be useful in some cases.

% In this case, some aspects of the result are known, while others are unknown. The variable could be in either term. 
% This clause determines which quantities represent each argument and then calls the actual procedure.
calc_format(output,Formula,[QtyCalc,UnitCalc],[[QtyR1,UnitR1],[QtyR2,UnitR2]],MaxSF) :-
	(var(QtyR1) -> calc_format_complex_result(Formula,[QtyCalc,UnitCalc],[QtyR2,UnitR2],[QtyR1,UnitR1],MaxSF);
	calc_format_complex_result(Formula,[QtyCalc,UnitCalc],[QtyR1,UnitR1],[QtyR2,UnitR2],MaxSF)).

calc_format_complex_result(Formula,[QtyCalc,UnitCalc],[QtyIn,UnitIn],[QtyOutRound,UnitOut],MaxSF) :-
	sigfigs(QtyIn,SFIn),
	to_number(QtyIn,QtyInNum),
	unit(Formula,[[QtyCalc,UnitCalc],[QtyInNum,UnitIn]],[QtyOut,UnitOut]),
	SF is min(MaxSF,SFIn),
	round_sigfigs(QtyOut,SF,QtyOutRound).


/*** NOTE:
STP is assumed to be calculated as defined by IUPAC: temperature of 0 °C and a pressure of 101.325 kPa.
The value used was obtained from CODATA: the NIST Reference on Constants, Units and Uncertainty.
***/
v_molar(22.413968 /* L/mol */).

%%% No conversion %%%
unit(_,[Mol,mol],[Mol,mol]) :- !.

%%% Mass units %%%
unit(Formula,[Mass,g],[Mol,mol]) :-
	molar_mass(Formula,MMass),
	Mol /* mol */ is Mass /* g */ * 1 /* mol */ / MMass /* g */, !.

unit(Formula,[Mol,mol],[Mass,g]) :-
	molar_mass(Formula,MMass),
	Mass /* g */ is Mol /* mol */ * MMass /* g */ / 1 /* mol */, !.

%%% Volume units %%%
/*** NOTE:
Formula is assumed to represent a gas at STP (standard temperature and pressure)
***/

unit(_,[Vol,'L'],[Mol,mol]) :-
	v_molar(Vm),
	Mol /* mol */ is Vol /* L */ * 1 /* mol */ / Vm /* L */, !.


unit(_,[Mol,mol],[Vol,'L']) :-
	v_molar(Vm),
	Vol /* L */ is Mol /* mol */ * Vm /* L */ / 1 /* mol */, !.


%%% Concentration units %%%
/*** NOTE:
 The volume (Vol) is assumed to represent the volume of the entire solution.
***/

unit(_,[[Vol,'L'],[Conc,'M']],[Mol,mol]) :-
	Mol /* mol */ is Conc /* mol / L */ * Vol /* L */, !.


unit(_,[[Mol,mol],[Conc,'M']],[Vol,'L']) :-
	Vol /* L */ is Mol /* mol */ / Conc /* mol / L */, !.

unit(_,[[Mol,mol],[Vol,'L']],[Conc,'M']) :-
	Conc /* M */ is Mol /* mol */ / Vol /* L */, !.

%%% Calculate the Molar Mass of a compound %%%
add_sum(Element,Sub,MassIn,MassOut) :-
	mass(Element,AtomicMass),!,
	MassOut is MassIn + AtomicMass * Sub.


sum_part([],_,X,X) :- !.
sum_part([[Sym,Sub]|PairS],GroupSub,Mass,MassF) :-
	add_sum(Sym, Sub * GroupSub,Mass,Mass1),
	sum_part(PairS,GroupSub,Mass1,MassF),
	!.

sum_part(Sym,Sub,Mass,MassF) :-
	add_sum(Sym, Sub, Mass, MassF).

molar_mass(Formula,MMass) :- molar_mass(Formula,0,MMass).

molar_mass([],X,X).

molar_mass([[Sym,Sub]|PairS],Mass,MassF) :-
	sum_part(Sym,Sub,Mass,Mass1),
	molar_mass(PairS,Mass1,MassF).
