%%% Covalent Compounds %%%%

covalent([Sym1,Sym2|Rest],Rest,Formula) --> covalent(Formula), {Formula = [[Sym1,_],[Sym2,_]]}.
covalent([Sym1,Sym2,Sym3|Rest],Rest,Formula) --> covalent(Formula), {Formula = [[Sym1,_],[Sym2,_],[Sym3,_]]}.

covalent(Formula) --> alk(Formula).
covalent(Formula) --> alcohol(Formula).

covalent(Formula) --> systematic_covalent(Formula).


%%% Systematic Naming for Covalent %%%

systematic_covalent([[Sym1,Num1],[Sym2,Num2]]) --> covalent_part(nonmetal,Sym1,sub_first,Num1)," ", (covalent_part(nonmetal_ide,Sym2,sub_general,Num2); syntax_stop(covalent_part)).



covalent_part(nonmetal_ide,"O",sub_general,2) --> "per", (nonmetal_ide("O",_,_); syntax_stop(peroxide_only)).



covalent_part(SymGoal,Sym,NumGoal,Num) --> call(NumGoal,Num,Letter), call(SymGoal,Sym,Matched,_),
(	{
	Letter = [] -> true;
	Matched = [H|_],
	(H = 'a'; H = 'o')
		} -> {true}; syntax_stop(vowel_required)).

	covalent_part(SymGoal,Sym,NumGoal,Num) --> (call(NumGoal,Num,Letter) -> {true}; syntax_stop(num_prefix)), Letter, call(SymGoal,Sym,Matched,_),
(	{
	Letter = [] -> true;
	Matched = [H|_],
	\+ (H = 'a'; H = 'o')
		} -> {true}; syntax_stop(vowel_omit)).


	covalent_nonmetal_ide(Sym,Base,Charge) --> (nonmetal_ide(Sym,Base,Charge) -> {true};  syntax_stop(nonmetal_ide)).


sub_first(Num,Letter) --> num_sub(Num,Letter).

/* CORRECTOR: remove if unecessary */
sub_first(Num,_) --> {var(Num)}, ("mono"; "mon"), syntax_stop(corrector_first_no_mono).
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


alk([["C",Num],["H",HydroNum]]) --> num_alk(Num), (alk_type(Num,HydroNum); syntax_stop(unknown_organic)).

alk_type(Num,HydroNum) -->  "ane",
	{
	HydroNum is 2 * Num + 2
	}.


alk_type(Num,HydroNum) --> "ene",
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
