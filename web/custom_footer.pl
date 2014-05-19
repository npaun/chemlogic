% custom_footer.pl: A footer with the Science Fair logo and link to to website.
% THIS FILE IS EDITABLE! (See style/custom_footer.css, as well)
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun

:- http_handler('/chemlogic/style/custom_footer.css',http_reply_file('style/custom_footer.css',[]),[]).
:- http_handler('/chemlogic/style/cwsf.png',http_reply_file('style/cwsf.png',[]),[]).



custom_footer --> html(ul(id(footlist),
        [
         li(img(src('style/cwsf.png'))),
        li(p(a(href('http://icebergsystems.ca/projects/chemlogic'),\['Nicholas Paun'])))
        ])).

