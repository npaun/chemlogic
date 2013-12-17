/**
NOTE: Simple oxyanions are generated from oxyanions/3 declarations, located with the element definitions.
TODO: Allow symbols for polyatomic groups to be given in human-readable notation, instead of internal format.
**/

%%%%% Anions %%%%%
cl_poly(group([["N",1],["H",4]],"ammonium","ammonium")).
        charge([["N",1],["H",4]],1).

cl_poly(group([["Hg",2]],"mercury(I)","mercury(I)")).
	charge([["Hg",2]],2).

cl_poly(group([["H",3],["O",1]],"hydronium","hydronium")).
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

cl_poly(group([["H",1],["C",1],["O",3]],"bicarbonate","")).
cl_poly(group([["H",1],["C",1],["O",3]],"hydrogen carbonate","")).
	charge([["H",1],["C",1],["O",3]],-1).

cl_poly(group([["H",1],["S",1],["O",4]],"bisulfate","")).
cl_poly(group([["H",1],["S",1],["O",4]],"hydrogen sulfate","")).
cl_poly(group([["H",1],["S",1],["O",4]],"bisulphate","")).
cl_poly(group([["H",1],["S",1],["O",4]],"hydrogen sulphate","")).
        charge([["H",1],["S",1],["O",4]],-1).

cl_poly(group([["H",1],["S",1],["O",3]],"bisulfite","")).
cl_poly(group([["H",1],["S",1],["O",3]],"hydrogen sulfite","")).
cl_poly(group([["H",1],["S",1],["O",3]],"bisulphite","")).
cl_poly(group([["H",1],["S",1],["O",3]],"hydrogen sulphite","")).
        charge([["H",1],["S",1],["O",3]],-1).

cl_poly(group([["H",1],["S",1]],"bisulfide","")).
cl_poly(group([["H",1],["S",1]],"hydrogen sulfide","")).
cl_poly(group([["H",1],["S",1]],"bisulphide","")).
cl_poly(group([["H",1],["S",1]],"hydrogen sulphide","")).
        charge([["H",1],["S",1]],-1).

cl_poly(group([["H",1],["C",2],["O",4]],"binoxalate","")).
cl_poly(group([["H",1],["C",2],["O",4]],"hydrogen oxalate","")).
	charge([["H",1],["C",2],["O",4]],-1).

cl_poly(group([["H",2],["P",1],["O",4]],"dihydrogen phosphate","")).
	charge([["H",2],["P",1],["O",4]],-1).


cl_poly(group([["H",1],["P",1],["O",4]],"monohydrogen phosphate","")).
	charge([["H",1],["P",1],["O",4]],-2).
