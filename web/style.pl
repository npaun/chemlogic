% style.pl: Styles the pages from the Web frontend: adds menus, containers and a CSS file
% This file is from Chemlogic, a logic programming computer chemistry system
% (C) Copyright 2012-2014 Nicholas Paun



:- multifile [user:head//2,user:body//2].




:- volatile style_dir/1.

find_style_dir :-
	prefix(Prefix),
	atom_concat(Prefix,'/share/chemlogic/',ChemlogicShare),
	exists_directory(ChemlogicShare) -> assertz(style_dir(ChemlogicShare));
	assertz(style_dir('')).


:- initialization(find_style_dir,now).


style_path(File,Path) :-
	style_dir(Dir),	
	atom_concat(Dir,File,Path).

% Load the custom footer

:- [custom_footer].


% Serves stylesheet and font.
:- initialization style_path('style/modern.css',P), http_handler('/chemlogic/style/modern.css',http_reply_file(P,[unsafe(true)]),[]).
:- initialization style_path('style/computer-modern.otf',P), http_handler('/chemlogic/style/computer-modern.otf',http_reply_file(P,[unsafe(true)]),[]).


% Injects stylesheet into every page.
user:head(chemlogic,Head) -->
	html(
	head([
	Head,
	link([rel(stylesheet),href('style/modern.css')]),
	link([rel(stylesheet),href('style/custom_footer.css')])
	])
	).

% Injects menu into every page.
user:body(chemlogic,Body) -->
	html(
	body([
	div(id(top),\cl_menu),
	div(id(content),Body),
 	div(id(footer),\custom_footer)
	])
	).

% The menu.
% TODO: Have each module register itself for the menu. Perhaps overkill.
cl_menu --> html(
	ul(id(menu),
	[
	li(h1(id(title),'Chemlogic')),
	li(a(href(compounder),'Compounder')),
	li(a(href(balancer),'Balancer')),
	li(span([class(todo),href(products),title('Not yet implemented.')],'Products')),
	li(span([class(todo),href(cplx_redox),title('Not yet implemented.')],'Complex Redox'))
	])).
