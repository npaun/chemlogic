%%%%% IUPAC Retained Names %%%%%

retained(["H","O"|Rest],Rest,[["H",2],["O",1]]) --> "water".
retained(["N","H"|Rest],Rest,[["N",1],["H",3]]) --> "ammonia".


%%%%% Common Names %%%%%

common(["Na","H","C","O"|Rest],Rest,[["Na",1],[[["H",1],["C",1],["O",3]],1]]) --> "baking soda".
common(["C","H","O"|Rest],Rest,[["C",3],["H",8],["O",1]]) --> "rubbing alcohol".
common(["C","H","O"|Rest],Rest,[["C",3],["H",8],["O",1]]) --> "surgical spirit".
