% style.pl: Styles the pages from the Web frontend: adds menus, containers and a CSS file
% This file is from Chemlogic, a logic programming computer chemistry system
% (C) Copyright 2012-2014 Nicholas Paun



:- multifile [user:head//2,user:body//2].





% Checks to see if style files have been installed in the correct system-wide location (e.g /usr/share/chemlogic)
% If not, look for style files in the current directory.

find_style_dir :-
	catch(prefix(Prefix),_,true), % Suppress errors when predicate is not yet configured.
	atom_concat(Prefix,'/share/chemlogic/',ChemlogicShare),
	exists_directory(ChemlogicShare) -> assertz(style_dir(ChemlogicShare));
	assertz(style_dir('')).

% This process must not be cached -- it is possible for Chemlogic to be moved.
:- volatile style_dir/1.
%  Locate style files once per start up
:- initialization(find_style_dir,now).


% This predicate will automatically register a HTTP handler for the style file, automatically calculating paths.
serve_style_file(File) :-
	style_dir(Dir),	
	atom_concat(Dir,File,FSPath),
	atom_concat('/chemlogic/',File,HTTPath),
	% The 'unsafe' option allows the HTTP server to serve a file above the directory from which Chemlogic was started.
	% This is necessary because style files may be installed in a system-wide location (e.g. /usr/share/chemlogic)
	% I think this is safe, because no user-provided input is used to determine which files to serve.
	http_handler(HTTPath,http_reply_file(FSPath,[unsafe(true)]),[]).

% Load the custom footer

:- [custom_footer].


% Serves stylesheet and font.
:- initialization serve_style_file('style/modern.css').
:- initialization serve_style_file('style/computer-modern.otf').


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
