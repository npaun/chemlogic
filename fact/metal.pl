% metal.pl: Database file for metals
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun



%%%%% Alkali Metals %%%%%

cl(element("Li","lithium","lith")).
	charge("Li",1).
	atomic("Li",3).
	mass("Li",6.941).

%% NOTE: natr is given as the base name (from natrium), but it is not used within the program.
cl(element("Na","sodium","natr")).
        charge("Na",1).
	atomic("Na",11).
	mass("Na",22.98977).

cl(element("K","potassium","kall")).
	charge("K",1).
	atomic("K",19).
	mass("K",39.0983).



%%%%% Alkaline Earth Metals %%%%%

cl(element("Mg","magnesium","magna")).
	charge("Mg",2).
	atomic("Mg",12).
	mass("Mg",24.305).

cl(element("Ca","calcium","calc")).
	charge("Ca",2).
	atomic("Ca",20).
	mass("Ca",40.08).

cl(element("Sr","strontium","stront")).
	charge("Sr",2).
	atomic("Sr",38).
	mass("Sr",87.62).

cl(element("Ba","barium","barium")).
	charge("Ba",2).
	atomic("Ba",56).
	mass("Ba",137.33).

%%%%% Actinides %%%%%

cl(element("U","uranium","uran")).
	charge("U",[6,4,5]).
	atomic("U",92).
	mass("U",238.029).

cl(element("Np","neptunium","neptun")).
	charge("Np",[5,3,4,6]).
	atomic("Np",93).
	mass("Np",237.048).

cl(element("Pu","plutonium","pluton")).
	charge("Pu",[4,6,3,5]).
	atomic("Pu",94).
	mass("Pu",244). % Mass number of the most stable or common isotope is used.

cl(element("Am","americium","americ")).
	charge("Am",[3,4,5,6]).
	atomic("Am",95).
	mass("Am",243). % Mass number of the most stable or common isotope is used.

%%%%% Transition Metals %%%%%

cl(element("V","vanadium","vanad")).
	charge("V",[5,4,3]).
	atomic("V",23).
	mass("V",50.9415).

cl(element("Cr","chromium","chrom")).
        charge("Cr",[2,3,6]).
	%% NOTE: There are two oxyanion declarations for chromium because the listed oxyanions for chromium have different charges.
        oxyanions("Cr",-2,[0,4,0,0]).
        oxyanions("Cr",-1,[0,0,2,0]).
	atomic("Cr",24).
	mass("Cr",51.996).

cl(element("Mn","manganese","mangan")).
	charge("Mn",[2,4]).
	oxyanions("Mn",-1,[4,0,0,0]).
	atomic("Mn",25).
	mass("Mn",54.9380).

cl(element("Fe","iron","ferr")).
	charge("Fe",[3,2]).
	atomic("Fe",26).
	mass("Fe",55.847).

cl(element("Co","cobalt","cobalt")).
	charge("Co",[2]).
	atomic("Co",27).
	mass("Co",58.9332).

cl(element("Cu","copper","cupr")).
        charge("Cu",[1,2]).
	atomic("Cu",29).
	mass("Cu",63.546).

cl(element("Zn","zinc","zinc")).
	charge("Zn",2).
	atomic("Zn",30).
	mass("Zn",65.39).

cl(element("Tc","technetium","technetium")).
	charge("Tc",7).
	atomic("Tc",43).
	mass("Tc",98). % Mass number of the most stable or common isotope is used.

cl(element("Ag","silver","argent")).
	charge("Ag",1).
	atomic("Ag",47).
	mass("Ag",107.868).

cl(element("W","tungsten","wolfram")).
	charge("W",[5,6]).
	atomic("W",74).
	mass("W",183.85).

cl(element("Re","rhenium","rhen")).
	charge("Re",[4,7]).
	atomic("Re",75).
	mass("Re",186.207).

cl(element("Au","gold","aurum")).
	charge("Au",[3,1]).
	atomic("Au",79).
	mass("Au",196.967).

cl(element("Hg","mercury","mercur")).
	charge("Hg",[2]).
	atomic("Hg",80).
	mass("Hg",200.59).

%%%%% Other Metals %%%%%

%% NOTE: Both aluminum and aluminium are accepted by the program. Output will use aluminum, first, but aluminium is available as an alternative solution.
cl(element("Al","aluminum","alum")).
cl(element("Al","aluminium","alum")).
        charge("Al",3).
	atomic("Al",13).
	mass("Al",26.98154).

cl(element("Sn","tin","stan")).
	charge("Sn",[4,2]).
	atomic("Sn",50).
	mass("Sn",118.71).

cl(element("Pb","lead","plumb")).
        charge("Pb",[2,4]).
	atomic("Pb",82).
	mass("Pb",207.2).
