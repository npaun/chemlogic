% nonmetal.pl: Database file for non-metals
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



% Longest match first. Unfortunately, the database must be stored out of order!
cl(element("Cl","chlorine","chlor")).
        charge("Cl",-1).
        oxyanions("Cl",-1,[4,3,2,1]).
        diatomic("Cl").
        atomic("Cl",17).
	mass("Cl",35.435).
	activity("Cl",200).


%%%%% Hydrogen %%%%%

cl(element("H","hydrogen","hydr")).
	%% NOTE: The two charges of hydrogen are given separately, instead of as a list, because hydrogen can act as both a metal and a non-metal. If both charges were in a list, hydrogen would be treated as a multivalent metal with charges I and -I.
%        charge("H",1).
        charge("H",-1).
        diatomic("H").
	atomic("H",1).
	mass("H",1.00794).
	activity("H",1300).

%%%%% Metalloids %%%%%

cl(element("Si","silicon","silic")).
	charge("Si",-2).
	atomic("Si",14).
	mass("Si",28.0855).

%%%%% Other Non-Metals %%%%%

cl(element("C","carbon","carbon")).
	%% NOTE: Carbon must be given a charge less than 0 for it to be treated as a non-metal, despite not forming ions. A charge of -1000 is used to prevent its use as an anion in an ionic compound.
        charge("C",-1000).
	%% TODO: This rule causes CO2 to be tested for being a carbonite ion. There needs to be some way to stop wasting time here.
        oxyanions("C",-2,[0,3,0,0]).
	atomic("C",6).
	mass("C",12.0111).

cl(element("N","nitrogen","nitr")).
        charge("N",-3).
        oxyanions("N",-1,[0,3,2,0]).
	atomic("N",7).
	mass("N",14.0067).
	
cl(element("O","oxygen","ox")).
        charge("O",-2).
        diatomic("O").
	atomic("O",8).
	mass("O",15.9994).

%% NOTE: phosphor is used as the base name when creating acids (e.g. phosphoric acid) instead of phosph (as in phosphate). This is handled in acid_base (ionic).
cl(element("P","phosphorus","phosph")).
        charge("P",-3).
        oxyanions("P",-3,[0,4,3,0]).
	atomic("P",15).
	mass("P",30.97376).

%% NOTE: sulfur is used as the base name when creating acids (e.g. sulfuric acid) instead of sulf (as in sulfate). This is handled in acid_base (ionic).
%% NOTE: Both sulfur and sulphur are accepted by the program. Output will use sulfur, preferrably, but sulphur is available as an alternative solution.
cl(element("S","sulfur","sulf")).
cl(element("S","sulphur","sulph")).
        charge("S",-2).
        oxyanions("S",-2,[0,4,3,0]).
	atomic("S",16).
	mass("S",32.06).

%%%%% Halogens %%%%%

cl(element("F","fluorine","fluor")).
	charge("F",-1).
	diatomic("F").
	atomic("F",9).
	mass("F",18.998403).
	activity("F",100).

cl(element("Br","bromine","brom")).
        charge("Br",-1).
        oxyanions("Br",-1,[4,3,2,1]).
        diatomic("Br").
	atomic("Br",35).
	mass("Br",79.904).
	activity("Br",300).

cl(element("I","iodine","iod")).
	charge("I",-1).
	oxyanions("I",-1,[4,3,2,1]).
	diatomic("I").
	atomic("I",53).
	mass("I",126.905).
	activity("I",400).

%%%%% Noble Gases %%%%%


cl(element("Xe","xenon","xen")).
	charge("Xe",0).
	atomic("Xe",54).
	mass("Xe",131.29).

