#!/usr/bin/env swipl
% main.pl: Server-independent code for the Chemlogic web interface
% This file is from Chemlogic, a logic programming computer chemistry system
% <http://icebergsystems.ca/chemlogic>
% (C) Copyright 2012-2014 Nicholas Paun



:- set_prolog_flag(verbose,silent).
:- set_prolog_flag(double_quotes,chars).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).


web_message(Port) :-
	writeln('\e[00;32mWelcome to the Chemlogic Web Interface!\e[00m'), nl,
	write('Try it out at \e[1mhttp://localhost:'), write(Port), writeln('/chemlogic/\e[0m'), nl,
	writeln('When you are done, type halt.'), nl, nl.

chemweb_to_html(String,HTML) :-
	atom_chars(Atom,String),
	HTML = \[Atom].

:- ensure_loaded('../build/compile.cf').
:- consult('style').
:- use_module(web_error).

:- ensure_loaded('../io/format_out').
:- initialization(set_output_format(html),now).

:- consult('../balance/balancer').

:- consult('compounder').
:- consult('balancer').

% vi: ft=prolog
