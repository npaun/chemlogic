% custom_footer.pl: A footer with the Science Fair logo and link to to website.
% THIS FILE IS EDITABLE! (See style/custom_footer.css, as well)
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2015 Nicholas Paun

:- initialization serve_style_file('style/custom_footer.css').
:- initialization serve_style_file('style/cwsf.png').



custom_footer --> html(ul(id(footlist),
        [
         li(img(src('style/cwsf.png'))),
        li(p(a(href('http://icebergsystems.ca/projects/chemlogic'),\['Nicholas Paun'])))
        ])).

