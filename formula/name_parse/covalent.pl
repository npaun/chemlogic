%%% Covalent Compounds %%%%

covalent([Sym1,Sym2|Rest],Rest,Formula) --> covalent(Formula), {Formula = [[Sym1,_],[Sym2,_]]}.
covalent([Sym1,Sym2,Sym3|Rest],Rest,Formula) --> covalent(Formula), {Formula = [[Sym1,_],[Sym2,_],[Sym3,_]]}.

covalent(Formula) --> alkane(Formula).
covalent(Formula) --> alkene(Formula).
covalent(Formula) --> alcohol(Formula).

covalent(Formula) --> systematic_covalent(Formula).


%%% Systematic Naming for Covalent %%%

systematic_covalent([[Sym1,Num1],[Sym2,Num2]]) --> covalent_part(non_metal,Sym1,sub_first,Num1)," ", covalent_part(non_metal_ide,Sym2,sub_general,Num2).


covalent_part(SymGoal,Sym,NumGoal,Num) --> call(NumGoal,Num,Letter), call(SymGoal,Sym,Matched),
	{
	Letter = [] -> true;
	Matched = [H|_],
	(H = 'a'; H = 'o')
	}.

covalent_part(SymGoal,Sym,NumGoal,Num) --> call(NumGoal,Num,Letter), Letter, call(SymGoal,Sym,_).

covalent_part(non_metal_ide,"O",2) --> "per", non_metal_ide("O",_).

sub_first(Num,Letter) --> num_sub(Num,Letter).
sub_first(1,"") --> "".

sub_general(Num,Letter) --> num_sub(Num,Letter).
sub_general(1,"o") --> "mon".

num_sub(2,[]) --> "di".
num_sub(3,[]) --> "tri".
num_sub(4,"a") --> "tetr".
num_sub(5,"a") --> "pent".
num_sub(6,"a") --> "hex".
num_sub(7,"a") --> "sept".
num_sub(8,"a") --> "oct".
num_sub(9,"a") --> "non".
num_sub(10,"a") --> "dec".

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
