% style.pl: Styles the pages from the Web frontend: adds menus, containers and a CSS file
% This file is from Chemlogic, a logic programming computer chemistry system
% (C) Copyright 2012-2014 Nicholas Paun



:- multifile [user:head//2,user:body//2].


% Load the custom footer

:- [custom_footer].


% Serves stylesheet and font.
:- http_handler('/chemlogic/style/modern.css',http_reply_file('style/modern.css',[]),[]).
:- http_handler('/chemlogic/style/computer-modern.otf',http_reply_file('style/computer-modern.otf',[]),[]).


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
