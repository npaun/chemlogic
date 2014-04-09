% retained.pl: Database file for IUPAC retained names and common names
% This file is from Chemlogic, a logic programming computer chemistry system
% (C) Copyright 2012-2014 Nicholas Paun

%%%%% IUPAC Retained Names (preferred) %%%%%

retained(["H","O"|Rest],Rest,[["H",2],["O",1]]) --> "water".
retained(["N","H"|Rest],Rest,[["N",1],["H",3]]) --> "ammonia".


%%%%% Common Names (recognized, but not produced) %%%%%

common(["Na","H","C","O"|Rest],Rest,[["Na",1],[[["H",1],["C",1],["O",3]],1]]) --> "baking soda".
common(["C","H","O"|Rest],Rest,[["C",3],["H",8],["O",1]]) --> "rubbing alcohol".
common(["C","H","O"|Rest],Rest,[["C",3],["H",8],["O",1]]) --> "surgical spirit".
common(["H","C","N"|Rest],Rest,[["H",1],[[["C",1],["N",1]],1]]) --> "prussic acid". %% Ionic form of CN is used so the acid recognizer will kick in.
common(["H","C","H","C","O","O"|Rest],Rest,[["H",1],[[["C",1],["H",3],["C",1],["O",1],["O",1]],1]]) --> "vinegar".
common(["O"|Rest],Rest,[["O",3]]) --> "ozone".
common(["Ca","C","O"|Rest],Rest,[["Ca",1],[[["C",1],["O",3]],1]]) --> "chalk".
common(["Ca","C","O"|Rest],Rest,[["Ca",1],[[["C",1],["O",3]],1]]) --> "limestone".
common(["Ca","O"|Rest],Rest,[["Ca",1],["O",1]]) --> "quicklime".
common(["Ca","O","H"|Rest],Rest,[["Ca",1],[[["O",1],["H",1]],2]]) --> "slaked lime".
common(["H","O"|Rest],Rest,[["H",2],["O",2]]) --> "hydrogen peroxide".
