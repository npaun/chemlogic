% format_out.pl: Formats various tokens depending on user selected output format
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun


:- dynamic output//2.



symbol(user,arrow) --> " --> ".
symbol(html,arrow) --> " &rarr; ".
symbol(html_android_textview,arrow) --> " &rarr; ".
symbol(latex,arrow) --> " \\rightarrow ".
symbol(ansi,arrow) --> " --> ".

symbol(user,sub_start) --> "".
symbol(user,sub_end) --> "".

symbol(html,sub_start) --> "<sub>".
symbol(html,sub_end) --> "</sub>".

symbol(html_android_textview,sub_start) --> "<sub>".
symbol(html_android_textview,sub_end) --> "</sub>".

symbol(latex,sub_start) --> "_{".
symbol(latex,sub_end) --> "}".

symbol(ansi,sub_start) --> "".
symbol(ansi,sub_end) --> "".


symbol(user,dot) --> ".".
symbol(html,dot) --> "&middot;".
symbol(html_android_textview,dot) --> "&middot;".
symbol(latex,dot) --> "\\cdot".
symbol(ansi,dot) --> ".".

symbol(user,err_token_start) --> "".
symbol(user,err_token_end) --> "".
/*
HTML error formatting is performed directly in the Web interface, to allow for CSS styling.
LaTeX formatting is not intended to be used in an user-interactive program, so error highlighting is probably not necessary.
	It may be added in the future.
*/

/* 
ANDROID: TextView supports only some HTML tags via the HTML class. The font tag can change the text's color (set to white), but not the background color (set to red).
A span with the inline background-color style is recognized by a customized tag handler that I have written for the app.
*/
symbol(html_android_textview,err_token_start) --> "<font color='#ffffff'><span style=\"background-color: #ff0000;\">".
symbol(html_android_textview,err_token_end) --> "</span></font>".

symbol(ansi,err_token_start) --> "\e[01;41;37m".
symbol(ansi,err_token_end) --> "\e[00m".


/** set_output_format(+Format) is det.

Causes side-effects. Causes all output from parsers to be in Format, unless overriden elsewhere.
*/
set_output_format(Format) :-
	debug_msg(set_output_format,['Output set to ',Format]),
	retractall(output(_,_,_,_)),
	expand_term((output(output,Symbol) --> symbol(Format,Symbol), !),DefaultRule),
	assertz(DefaultRule),
	expand_term((output(Other,Symbol) --> symbol(Other,Symbol),!),OtherRule),
	assertz(OtherRule).

:- initialization set_output_format(user).
