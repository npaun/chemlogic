:- set_prolog_flag(double_quotes,chars).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- initialization server(8000,[]).


server(Port, Options) :-
        http_server(http_dispatch,
                    [ port(Port)
                    | Options
                    ]).

:- include('style.pl').


:- include('../balance/balancer').

:- include('compounder.pl').
:- include('balancer.pl').
