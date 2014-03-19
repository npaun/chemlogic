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

cl_poly(group([["C",1],[[["N",1],["H",2]],3]],"guanidinium","guanidinium")).
	charge([["C",1],[[["N",1],["H",2]],3]],1).

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

cl_poly(group([["H",1],["C",1],["O",3]],"bicarbonate","x")).
cl_poly(group([["H",1],["C",1],["O",3]],"hydrogen carbonate","x")).
	charge([["H",1],["C",1],["O",3]],-1).

cl_poly(group([["H",1],["S",1],["O",4]],"bisulfate","x")).
cl_poly(group([["H",1],["S",1],["O",4]],"hydrogen sulfate","x")).
cl_poly(group([["H",1],["S",1],["O",4]],"bisulphate","x")).
cl_poly(group([["H",1],["S",1],["O",4]],"hydrogen sulphate","x")).
        charge([["H",1],["S",1],["O",4]],-1).

cl_poly(group([["H",1],["S",1],["O",3]],"bisulfite","x")).
cl_poly(group([["H",1],["S",1],["O",3]],"hydrogen sulfite","x")).
cl_poly(group([["H",1],["S",1],["O",3]],"bisulphite","x")).
cl_poly(group([["H",1],["S",1],["O",3]],"hydrogen sulphite","x")).
        charge([["H",1],["S",1],["O",3]],-1).

cl_poly(group([["H",1],["S",1]],"bisulfide","x")).
cl_poly(group([["H",1],["S",1]],"hydrogen sulfide","x")).
cl_poly(group([["H",1],["S",1]],"bisulphide","x")).
cl_poly(group([["H",1],["S",1]],"hydrogen sulphide","x")).
        charge([["H",1],["S",1]],-1).

cl_poly(group([["H",1],["C",2],["O",4]],"binoxalate","x")).
cl_poly(group([["H",1],["C",2],["O",4]],"hydrogen oxalate","x")).
	charge([["H",1],["C",2],["O",4]],-1).

cl_poly(group([["H",2],["P",1],["O",4]],"dihydrogen phosphate","x")).
	charge([["H",2],["P",1],["O",4]],-1).


cl_poly(group([["H",1],["P",1],["O",4]],"monohydrogen phosphate","x")).
	charge([["H",1],["P",1],["O",4]],-2).
