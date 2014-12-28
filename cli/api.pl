:- op(990,xfy,::).
:- op(100,xf,print).

InputType - Input :: print :-
        InputType - Input :: symbolic - Result,
        writeln(Result),
        !.

InputType - Input :: ResultType print :-
        InputType - Input :: ResultType - Result,
        writeln(Result),
        !.

cli_api_init([]) :- cli_api_init([user]).
cli_api_init([Format]) :-
        set_output_format(Format),
        assertz(echo_output(X) :- writeln(X)),
        set_stream(user_input,tty(true)),
        set_stream(user_output,tty(true)),
        set_prolog_flag(toplevel_prompt,'CL ?-[~!]\n').

cli_api_message :-
        writeln('Chemlogic      API').

