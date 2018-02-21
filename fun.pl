%% -*- prolog -*-

:- module(fun,
          [
          ]).

:- use_module(library(http/http_open)).
:- use_module(discuss).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun_answer(List, _, Answer) :-
    member("fun", List),
    member("help", List),
    format(string(Answer), "~n~w~n~w", ["fortune <word>: fortune message", "weather <location>: weather at location"]).

fun_answer(["fortune"|Args], _, Answer) :-
    string_join(" ", Args, ArgsText),
    cmd("fortune ~w", ArgsText, Lines, _),
    string_join("\n", Lines, Answer).

fun_answer(["weather", Location], _, "Good Moon") :-
    string_lower(Location, "moon").

fun_answer(["weather", Location], _, Answer) :-
    format(string(WttrUrl), "http://wttr.in/~w?0T", [Location]),
    setup_call_cleanup(
        http_open(WttrUrl, In, [user_agent("curl/7.55.1")]),
        load_html(In, [Answer], []),
        close(In)
    ).

:- add_answerer(fun:fun_answer).

%% fun.pl ends here
