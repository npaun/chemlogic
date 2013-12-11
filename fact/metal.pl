%%%%% Alkali Metals %%%%%

%% NOTE: natr is given as the base name (from natrium), but it is not used within the program.
cl(element("Na","sodium","natr")).
        charge("Na",1).
	atomic("Na",11).




%%%%% Alkaline Earth Metals %%%%%

%%%%% Transition Metals %%%%%


cl(element("Cr","chromium","chrom")).
        charge("Cr",[2,3]).
	%% NOTE: There are two oxyanion declarations for chromium because the listed oxyanions for chromium have different charges.
        oxyanions("Cr",-2,[0,4,0,0]).
        oxyanions("Cr",-1,[0,0,2,0]).
	atomic("Cr",24).


cl(element("Cu","copper","cupr")).
        charge("Cu",[1,2]).
	atomic("Cu",29).

%%%%% Other Metals %%%%%

%% NOTE: Both aluminum and aluminium are accepted by the program. Output will use aluminum, first, but aluminium is available as an alternative solution.
cl(element("Al","aluminum","alum")).
cl(element("Al","aluminium","alum")).
        charge("Al",3).
	atomic("Al",13).

cl(element("Pb","lead","plumb")).
        charge("Pb",[2,4]).
	atomic("Pb",82).

