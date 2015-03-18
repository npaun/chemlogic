:- consult('../balance/balancer').
:- use_module(sigfigs).

stoich(CoeffIn,FormulaIn,QtyIn,CoeffOut,FormulaOut,QtyOut) :-
	calc(FormulaIn,QtyIn,[MolIn,mol],SF),
	MolOut is MolIn * CoeffOut / CoeffIn,
	calc(FormulaOut,[MolOut,mol],QtyOut,SF).

calc(Formula,[QtyIn,UnitIn],[QtyOutR,UnitOut],MaxSF) :-
	(atom(QtyIn) -> atom_number(
	unit(Formula,[QtyIn,UnitIn],[QtyOut,UnitOut]),
	(var(MaxSF) -> 
		(
			sf_calc(QtyIn,MaxSF),
			QtyOut = QtyOutR
		);
		(
			sf_produce(QtyOut,MaxSF,QtyOutR)
		)
	).
		




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

unit(_,[Vol,'L',Conc,'M'],[Mol,mol],LowestSF) :-
	sf_calc(Vol,VolSF),
	sf_calc(Conc,ConcSF),
	LowestSF is min(VolSF,ConcSF),
	Mol /* mol */ is Conc /* mol / L */ * Vol /* L */, !.


%unit(_,[Mol,mol,Conc,'M'],[Vol,'L']) :-
%	Vol /* L */ is Mol /* mol */ / Conc /* mol / L */, !.

unit(_,[Mol,mol],[Vol,'L',Conc,'M'],LowestSF) :-
	var(Conc) -> 
	sf_calc(Mol,MolSF),
	sf_calc(Vol,VolSF),
	LowestSF is min(MolSF,VolSF),
	Conc /* M */ is Mol /* mol */ / Vol /* L */, !.

unit(_,[Mol,mol],[Vol,'L',Conc,'M'],LowestSF) :-
	var(Vol) ->( 
	sf_calc(Mol,MolSF,MolNum),
	sf_calc(Conc,ConcSF,ConcNum),
	LowestSF is min(MolSF,ConcSF),
	writeln(LowestSF),
	Vol /* L */ is MolNum /* mol */ / ConcNum /* mol / L */, !).


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
