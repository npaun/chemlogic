% polyatomic.pl: Database file for polyatomic ions (except oxyanions)
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun



/**
NOTE: Simple oxyanions are generated from oxyanions/3 declarations, located with the element definitions.
TODO: Allow symbols for polyatomic groups to be given in human-readable notation, instead of internal format.
**/

%%%%% Anions %%%%%
cl_poly(group([["N",1],["H",4]],"ammonium","n/a")).
        charge([["N",1],["H",4]],1).

cl_poly(group([["Hg",2]],"mercury(I)","n/a")).
	charge([["Hg",2]],2).

cl_poly(group([["H",3],["O",1]],"hydronium","n/a")).
	charge([["H",3],["O",1]],1).

%%%%% Cations %%%%%
cl_poly(group([["O",1],["H",1]],"hydroxide","hydrox")).
        charge([["O",1],["H",1]],-1).

cl_poly(group([["C",1],["N",1]],"cyanide","cyan")).
        charge([["C",1],["N",1]],-1).

cl_poly(group([["S",1],["C",1],["N",1]],"thiocyanate","thiocyan")).
        charge([["S",1],["C",1],["N",1]],-1).

cl_poly(group([["C",1],["H",3],["C",1],["O",1],["O",1]],"acetate","acet")).
cl_poly(group([["C",1],["H",3],["C",1],["O",1],["O",1]],"ethanoate","ethano")).
	charge([["C",1],["H",3],["C",1],["O",1],["O",1]],-1).

cl_poly(group([["C",2],["O",4]],"oxalate","oxal")).
	charge([["C",2],["O",4]],-2).

cl_poly(group([["Cr",2],["O",7]],"dichromate","dichrom")).
	charge([["Cr",2],["O",7]],-2).

%%%%% hydrogen <x> and bi<x> %%%%%

cl_poly(group([["H",1],["C",1],["O",3]],"hydrogen carbonate","n/a")).
cl_poly(group([["H",1],["C",1],["O",3]],"bicarbonate","n/a")).
	charge([["H",1],["C",1],["O",3]],-1).

cl_poly(group([["H",1],["S",1],["O",4]],"hydrogen sulfate","n/a")).
cl_poly(group([["H",1],["S",1],["O",4]],"bisulfate","n/a")).
cl_poly(group([["H",1],["S",1],["O",4]],"hydrogen sulphate","n/a")).
cl_poly(group([["H",1],["S",1],["O",4]],"bisulphate","n/a")).
        charge([["H",1],["S",1],["O",4]],-1).

cl_poly(group([["H",1],["S",1],["O",3]],"hydrogen sulfite","n/a")).
cl_poly(group([["H",1],["S",1],["O",3]],"bisulfite","n/a")).
cl_poly(group([["H",1],["S",1],["O",3]],"hydrogen sulphite","n/a")).
cl_poly(group([["H",1],["S",1],["O",3]],"bisulphite","n/a")).
        charge([["H",1],["S",1],["O",3]],-1).

cl_poly(group([["H",1],["S",1]],"hydrogen sulfide","n/a")).
cl_poly(group([["H",1],["S",1]],"bisulfide","n/a")).
cl_poly(group([["H",1],["S",1]],"hydrogen sulphide","n/a")).
cl_poly(group([["H",1],["S",1]],"bisulphide","n/a")).
        charge([["H",1],["S",1]],-1).

cl_poly(group([["H",1],["C",2],["O",4]],"hydrogen oxalate","n/a")).
cl_poly(group([["H",1],["C",2],["O",4]],"hydrogenoxalate","n/a")).
cl_poly(group([["H",1],["C",2],["O",4]],"binoxalate","n/a")).
	charge([["H",1],["C",2],["O",4]],-1).

cl_poly(group([["H",2],["P",1],["O",4]],"dihydrogen phosphate","n/a")).
	charge([["H",2],["P",1],["O",4]],-1).


cl_poly(group([["H",1],["P",1],["O",4]],"monohydrogen phosphate","n/a")).
	charge([["H",1],["P",1],["O",4]],-2).
