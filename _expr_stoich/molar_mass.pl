:- consult('../balance/balancer').
:- use_module(sigfigs).

stoich(CoeffK,FormulaK,QtyK,CoeffR,FormulaR,QtyR) :-
	calc(user,FormulaK,QtyK,[MolK,mol],SF),
	MolR is MolK * CoeffR / CoeffK,
	calc(output,FormulaR,[MolR,mol],QtyR,SF).

		
calc(user,Formula,[QtyIn,UnitIn],[QtyOut,UnitOut],SF) :-
	sf_calc(QtyIn,SF),
	number_chars(QtyInNum,QtyIn),
	unit(Formula,[QtyInNum,UnitIn],[QtyOut,UnitOut]).

calc(output,Formula,[QtyIn,UnitIn],[QtyOutRound,UnitOut],SF) :-
	unit(Formula,[QtyIn,UnitIn],[QtyOut,UnitOut]),
	sf_produce(QtyOut,SF,QtyOutRound).


calc(user,Formula,[[QtyIn1,UnitIn1],[QtyIn2,UnitIn2]],[QtyOut,UnitOut],SF) :-
	sf_calc(QtyIn1,SF1),
	number_chars(QtyInNum1,QtyIn1),
	sf_calc(QtyIn2,SF2),
	number_chars(QtyInNum2,QtyIn2),
	SF is min(SF1,SF2),
	unit(Formula,[[QtyInNum1,UnitIn1],[QtyInNum2,UnitIn2]],[QtyOut,UnitOut]).

/* Perhaps output might be useful from a two unit input? */

calc(output,Formula,[QtyCalc,UnitCalc],[[QtyIn,UnitIn],[QtyOutRound,UnitOut]],MaxSF) :-
	sf_calc(QtyIn,SFIn),
	number_chars(QtyInNum,QtyIn),
	unit(Formula,[[QtyCalc,UnitCalc],[QtyInNum,UnitIn]],[QtyOut,UnitOut]),
	SF is min(MaxSF,SFIn),
	sf_produce(QtyOut,SF,QtyOutRound).


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
