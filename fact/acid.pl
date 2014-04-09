% acid.pl: Contains special cases relevant to naming acids
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun



acid_ion_suffix(Elem) -->
	{Elem = "S"} -> "ur";
        {Elem = "P"} ->	"or";
	"".



% vi: ft=prolog
