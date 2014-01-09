%%% Covalent Compounds %%%%

covalent([Sym,Sym2|Rest],Rest,F) --> covalent(F), {F = [[Sym,_],[Sym2,_]]}.
covalent([Sym,Sym2,Sym3|Rest],Rest,F) --> covalent(F), {F = [[Sym,_],[Sym2,_],[Sym3,_]]}.

covalent(F) --> alkane(F).
covalent(F) --> alkene(F).
covalent(F) --> alcohol(F).

covalent(F) --> systematic_covalent(F).


%%% Systematic Naming for Covalent %%%	

systematic_covalent([[Sym,Num],[Sym2,Num2]]) --> covalent_part(non_metal,Sym,Num)," ", covalent_part(non_metal_ide,Sym2,Num2).


covalent_part(Goal,Sym,Num) --> num_sub(Num,Letter), call(Goal,Sym,Matched),
	{
	 Letter = [] -> true;
	 Matched = [H|_],
	 	(H = 'a'; H = 'o')
	}.

covalent_part(Goal,Sym,Num) --> num_sub(Num,Letter), Letter, call(Goal,Sym,_).

covalent_part(non_metal_ide,"O",2) --> "per", non_metal_ide("O",_).

num_sub(2,[]) --> "di".
num_sub(3,[]) --> "tri".
num_sub(4,"a") --> "tetr".
num_sub(5,"a") --> "pent".
num_sub(6,"a") --> "hex".
num_sub(7,"a") --> "sept".
num_sub(8,"a") --> "oct".
num_sub(9,"a") --> "non".
num_sub(10,"a") --> "dec".

num_sub(1,[]) --> [].
num_sub(1,"o") --> "mon".

%%% Alkanes, Alkenes and Alcohols %%%


alkane([["C",Num],["H",HydroNum]]) --> num_alk(Num), "ane",
	{
	 HydroNum is 2 * Num + 2
	}.


alkene([["C",Num],["H",HydroNum]]) --> num_alk(Num), "ene",
	{
	 HydroNum is 2 * Num
	}.

alcohol([["C",Num],["H",HydroNum],["O",1]]) --> num_alk(Num), "anol",
	{
	 HydroNum is 2 * Num + 2
	}.

num_alk(1) --> "meth".
num_alk(2) --> "eth".
num_alk(3) --> "prop".
num_alk(4) --> "but".

num_alk(5) --> "pent".
num_alk(6) --> "hex".
num_alk(7) --> "sept".
num_alk(8) --> "oct".
num_alk(9) --> "non".
num_alk(10) --> "dec".
