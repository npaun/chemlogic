:- multifile [user:head//2,user:body//2].

% Serves stylesheet and font.
:- http_handler('/chemlogic/style/modern.css',http_reply_file('style/modern.css',[]),[]).
:- http_handler('/chemlogic/style/computer-modern.otf',http_reply_file('style/computer-modern.otf',[]),[]).


% Injects stylesheet into every page.
user:head(chemlogic,Head) -->
	html(
	head([
	Head,
	link([rel(stylesheet),href('style/modern.css')])
	])
	).

% Injects menu into every page.
user:body(chemlogic,Body) -->
	html(
	body([
	div(id(top),\cl_menu),
	div(id(content),Body)
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
	li(span([class(todo),href(products)],'Products')),
	li(span([class(todo),href(cplx_redox)],'Complex Redox'))
	])).
