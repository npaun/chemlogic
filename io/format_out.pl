% format_out.pl: Formats various tokens depending on user selected output format
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun


:- dynamic output//2.



symbol(user,arrow) --> " --> ".
symbol(html,arrow) --> " &rarr; ".
symbol(latex,arrow) --> " \\rightarrow ".
symbol(ansi,arrow) --> " -->".

symbol(user,sub_start) --> "".
symbol(user,sub_end) --> "".

symbol(html,sub_start) --> "<sub>".
symbol(html,sub_end) --> "</sub>".

symbol(latex,sub_start) --> "_{".
symbol(latex,sub_end) --> "}".

symbol(ansi,sub_start) --> "".
symbol(ansi,sub_end) --> "".


symbol(user,dot) --> ".".
symbol(html,dot) --> "&middot;".
symbol(latex,dot) --> "\\cdot".
symbol(ansi,dot) --> ".".

symbol(user,err_token_start) --> "".
symbol(user,err_token_end) --> "".
/*
HTML error formatting is performed directly in the Web interface, to allow for CSS styling.
LaTeX formatting is not intended to be used in an user-interactive program, so error highlighting is probably not necessary.
	It may be added in the future.
*/

symbol(ansi,err_token_start) --> "\e[01;41;37m".
symbol(ansi,err_token_end) --> "\e[00m".


/** set_output_format(+Format) is det.

Causes side-effects. Causes all output from parsers to be in Format, unless overriden elsewhere.
*/
set_output_format(Format) :-
	debug(chemlogic_custom,'Set ~w~n',[Format]),
	retractall(output(_,_,_,_)),
	dcg_translate_rule((output(output,Symbol) --> symbol(Format,Symbol), !),DefaultRule),
	assertz(DefaultRule),
	dcg_translate_rule((output(Other,Symbol) --> symbol(Other,Symbol),!),OtherRule),
	assertz(OtherRule).

:- initialization set_output_format(user).
