% TODO  
% This file is from Chemlogic, a logic programming computer chemistry system  
% <http://icebergsystems.ca/chemlogic>  
% (C) Copyright 2012-2014 Nicholas Paun  


## Contents ##
1. Science 10
2. Chemistry concepts
3. Database
4. Program features
5. Organization and Structure
6. Bugs



## 1. Science 10 ##

Chemlogic currently implements most of the Chemistry relating to equations and compounds from the Science 10 curriculum. It is missing one major feature, however:

* Recognizing reaction types
	* There are very many reaction types, but the program should recognize the most common ones (synthesis, decomposition, single replacement, double replacement, neutralization and combustion)
	* Probably another module will be necessary that uses equation structures, fits them according to the type and then fills in the products



## 2. Chemistry features ##

* Support for structural formulas:
	* The covalent parser will have to be extended very much, to handle structures of the compounds it supports
	* The formula parser will need to have some sort of input and output representation for structural formulas
	* Each output format will have its own ways of rendering structural formulas. This will have to be extended.
	* A module will be needed to convert structural formulas to molecular formulas for balancing and other processes.

* Reactivity series:
	* After the types of reactions are recognized, it will not be too difficult to implement knowledge about whether or not a reaction will take place. The user can be informed about this when an equation is balanced.

* Mole calculations:
	* A field will need to be added in the database for atomic mass of elements.
	* `fact_dcg_translate` will need to be extended to handle polyatomic groups/oxyanions
	* Tabulate or possibly `formula_normalize` will be extended to calculate molar masses
	* Allow parsers to accept information about stoichiometric problems to solve
	* Implement information about mole bridges into the solver (probably a new module)

* More organic naming:
	* It will be useful to implement organic compound naming at least for Chemistry 11 to 12.

* Complex Redox reactions:
	* Sometimes, for a few very complex redox reactions, Chemlogic gives an answer that satisfies the system of linear equations (i.e. is balanced) but will not actually occur in real life.
	* A new balancing process, with a separate module should be implemented. Perhaps based on oxidation numbers or half-reactions

* Diagramming:
	* Show structural formulas of compounds
	* Bohr models, Lewis diagrams
	* Periodic tables?




## 3. Database ##

* Add many, many more elements.
* More polyatomic groups
* Write-up a large batch of common names
* Add atomic masses




## 4. Program features ##

* Extend the `chemcli` DSL to make it more useful.
	* Offer a way to query the chemical information database.
	* More constructs/operators.
	* A standard library?
	* This will all depend on the sorts of programs that someone will actually want to write

* Quiz program
	* Allow for questions to be generated with selectable options (the parser and perhaps major subparts)
	* Interactive and non-interactive usage depending on output format
	* Configurable marking (allow retry, show correct answer at end, etc.)
	* Possible to produce the same questions if passed the same seed.
	* Some sort of intelligence, focusing on problem areas when giving questions.

* Expose the chemical information database.

* Better error messages for equation balancing errors
	* Test to ensure that all elements appear in both products and reactants
	* Perhaps explain why some charge shifts are unsatisfiable
	* Explain which element makes the system unbalancable, if possible.




## 5. Organization and Structure ##

* Take advantage of SWI-Prolog's new string type. This makes tokenization, concatenation and many other things more efficient.
* Chemlogic needs a simple tokenizer to break up chemical names and equations (especiallly)
	* At minimum, it should split on spaces and remove extraneous spaces
	* Potentially deal with character types?
	* Potentially deal with the insides of parentheses?
* Rewrite the current error tokenizers to use the new and better functions
* Tokenizers make parser much, much nicer:
	* It is now quick and simple to see if something is an acid without having to go through all of the tests
	* It can be easy to distinguish between ionic and covalent


* Make the oxyanion functions less messy and hacky. There must be a better way to tell the user what's wrong with the oxyanion names.
* The ugly hacks around pure substances can be removed with a better tokenizer

* Some things will need to be renamed and reorganized

* Use more meta-programming to remove boilerplate code from the web interface.



## 6. Bugs ##

* The program will get very upset if a substance is repeated:
	* e.g. `H2O + H2O --> H2O`
	* There is not much of a valid reason to enter this, but the program should handle this correctly
	* An error message explaining that this is junk is probably a good idea

