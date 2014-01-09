:- multifile [user:head//2,user:body//2].

:- http_handler('/chemlogic/style/modern.css',http_reply_file('style/modern.css',[]),[]).
:- http_handler('/chemlogic/style/computer-modern.otf',http_reply_file('style/computer-modern.otf',[]),[]).


user:head(chemlogic,Head) -->
	html(
		head([
			Head,
			link([rel(stylesheet),href('style/modern.css')])
			])
		).

user:body(chemlogic,Body) -->
	html(
		body([
			div(id(top),\cl_menu),
			div(id(content),Body)
			])
		).

cl_menu --> html(
	ul(id(menu),
	[
	li(h1(id(title),'Chemlogic')),
	li(a(href(compounder),'Compounder')),
	li(a(href(balancer),'Balancer')),
	li(a([class(todo),href(products)],'Products')),
	li(a([class(todo),href(cplx_redox)],'Complex Redox'))
	])).
