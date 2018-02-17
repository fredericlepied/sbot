%% -*- prolog -*-

:- module(fun,
          [
          ]).

:- use_module(discuss).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun_answer(List, _, "fortune <word>: furtune message") :-
    member("fun", List),
    member("help", List).

fun_answer(["fortune"|Args], _, Answer) :-
    string_join(" ", Args, ArgsText),
    cmd("fortune ~w", ArgsText, Lines, _),
    string_join("\n", Lines, Answer).

:- add_answerer(fun:fun_answer).

%% fun.pl ends here
