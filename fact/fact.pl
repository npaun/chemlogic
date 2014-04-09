% fact.pl: The main file that loads the chemical information database
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun



/**
NOTE: Quick description of Prolog notation.

Predicates (like a subroutine or function) are referred to by their name and arity (number of arguments or parameters).
e.g element/3.

If an arity is separated from the name by 2 slashes (formula//3), it is a grammatical rule, and its actual arity is n + 2 (formula/5).

Predicates are defined with a name and arguments (generally either a variable (starts with a capital letter) or some kind of a term (usually lowercase)):
element(Symbol,Name,Base)

The following symbols are used as prefixes for a variable:
+	This argument must be instantiated (it is input for the predicate).
-	This argument must not be instantiated (it is output for the predicate).
?	This argument can be input or output, depending on which function of the predicate is desired.

The definition of element/3 is:
element(+Symbol,+Name,+Base)
**/

/**
REFERENCE: chemlogic fact database

Each element is defined by an element/3 fact:
element(+Symbol,+Name,+Base)

Symbol	string		The symbol of the element
Name	string		The full name of the element
Base	string		The base of the element, used when the name is combined with a modifier

This fact must be given as an argument to cl/1, which converts the fact into grammatical rules.

For example:
cl(element("H","hydrogen","hydr").

***

Polyatomic groups are defined by a group/3 fact:
group(+Formula,+Name,+Base)

Formula	list (formula//3 struct)	The formula of the polyatomic ion
Name	string				The name of the polyatomic ion
Base	string				Not used

This fact must be given as an argument to cl_poly/1, which converts the fact into grammatical rules.

For example:
cl_poly(group(["N",1,"H",4],"ammonium","ammonium")).

***

Elements and polyatomic groups have their charges listed by a charge/2 fact:
charge(+Symbol,+Charges).

Symbol/Formula	list/string	The symbol of the element/polyatomic group
Charges		number/list	The charge (or charges) of the element/ion.	

Multivalent metals are detected by checking for a list for Charges, so do not use it for cases where there are multiple charges on something other than a multivalent metal.


***

Elements may have their oxyanions defined by a oxyanions/3 fact:
oxyanions(+Symbol,+Charge,+[PerAte,Ate,Ite,HypoIte]).

Symbol	string	The symbol of the element
Charge	number	The charge on all of the defined oxyanions
PerAte,Ate,Ite,HypoIte	number	The number of oxygen atoms in each of the oxyanions (0 if the the oxyanion does not exist).


***

Elements have their atomic number defined by a atomic/2 fact:
atomic(+Symbol,+Number)

Symbol	string	The symbol of the element
Number	number	The atomic number of the element

***

Diatomic elements are indicated by a diatomic/1 fact:
diatomic(+Symbol)

Symbol	string	The symbol of the element

**/

:- discontiguous [cl/1,cl_poly/1]. % The chemlogic DCG translating rules.
:- discontiguous [charge/2,oxyanions/3,atomic/2,diatomic/1]. % Facts about an element or group
:- dynamic [charge/2].

:- include('metal').
:- include('nonmetal').
:- include('polyatomic').

% Retained and common names for compounds
:- include('retained').

% A different base name is used for sulfur and phosphorus when forming acids
:- include('acid').


%%%% DATABASE VERSION %%%%

db_version(2014,04,09,00).
