%%%%% Alkali Metals %%%%%

cl(element("Li","lithium","lith")).
	charge("Li",1).
	atomic("Li",3).

%% NOTE: natr is given as the base name (from natrium), but it is not used within the program.
cl(element("Na","sodium","natr")).
        charge("Na",1).
	atomic("Na",11).

cl(element("K","potassium","kall")).
	charge("K",1).
	atomic("K",19).

cl(element("Ba","barium","barium")).
	charge("Ba",2).
	atomic("Ba",56).



%%%%% Alkaline Earth Metals %%%%%

cl(element("Mg","magnesium","magna")).
	charge("Mg",2).
	atomic("Mg",12).

cl(element("Ca","calcium","calc")).
	charge("Ca",2).
	atomic("Ca",20).


%%%%% Transition Metals %%%%%


cl(element("Cr","chromium","chrom")).
        charge("Cr",[2,3]).
	%% NOTE: There are two oxyanion declarations for chromium because the listed oxyanions for chromium have different charges.
        oxyanions("Cr",-2,[0,4,0,0]).
        oxyanions("Cr",-1,[0,0,2,0]).
	atomic("Cr",24).

cl(element("Mn","manganese","mangan")).
	charge("Mn",[2,4]).
	oxyanions("Mn",-1,[4,0,0,0]).
	atomic("Mn",25).

cl(element("Fe","iron","ferr")).
	charge("Fe",[3,2]).
	atomic("Fe",26).

cl(element("Cu","copper","cupr")).
        charge("Cu",[1,2]).
	atomic("Cu",29).

cl(element("Zn","zinc","zinc")).
	charge("Zn",2).
	atomic("Zn",30).

cl(element("Ag","silver","argent")).
	charge("Ag",1).
	atomic("Ag",47).

cl(element("Au","gold","aurum")).
	charge("Au",[3,1]).
	atomic("Au",79).

cl(element("Hg","mercury","mercur")).
	charge("Hg",[2]).
	atomic("Hg",80).

%%%%% Other Metals %%%%%

%% NOTE: Both aluminum and aluminium are accepted by the program. Output will use aluminum, first, but aluminium is available as an alternative solution.
cl(element("Al","aluminum","alum")).
cl(element("Al","aluminium","alum")).
        charge("Al",3).
	atomic("Al",13).

cl(element("Sn","tin","stan")).
	charge("Sn",[4,2]).
	atomic("Sn",50).

cl(element("Pb","lead","plumb")).
        charge("Pb",[2,4]).
	atomic("Pb",82).
