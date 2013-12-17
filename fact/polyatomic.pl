/**
NOTE: Simple oxyanions are generated from oxyanions/3 declarations, located with the element definitions.
TODO: Allow symbols for polyatomic groups to be given in human-readable notation, instead of internal format.
**/

%%%%% Anions %%%%%
cl_poly(group([["N",1],["H",4]],"ammonium","ammonium")).
        charge([["N",1],["H",4]],1).

%%%%% Cations %%%%%
cl_poly(group([["O",1],["H",1]],"hydroxide","hydrox")).
        charge([["O",1],["H",1]],-1).

cl_poly(group([["C",1],["N",1]],"cyanide","cyan")).
        charge([["C",1],["N",1]],-1).

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

cl_poly(group([["C",1],["H",3],["C",1],["O",1],["O",1]],"acetate","acet")).
	charge([["C",1],["H",3],["C",1],["O",1],["O",1]],-1).
