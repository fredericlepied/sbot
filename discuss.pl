%% -*- prolog -*-

:- module(discuss, [process_message]).

:- use_module(library(irc_client_utilities)).
:- use_module(library(irc_client_parser)).

process_message(Id, Server, "PRIVMSG", Params, Text) :-
    member("sbot", Params),
    prefix_id(Server, Nick, U, H),
    writeln(["DIRECT", Code, Nick, U, H, Param, Text]),
    private_message(Id, Text, Nick).
    
process_message(Id, Server, "PRIVMSG", [Param|_], Text) :-
    split_string(Text, " ", "@:", S),
    member("sbot", S),
    prefix_id(Server, Nick, U, H),
    writeln([Code, Nick, U, H, Param, Text]),
    public_message(Id, Text, Nick, Param).

process_message(Id, Server, Code, Params, Text) :-
    prefix_id(Server, N, U, H),
    writeln(["UNKNOWN", Code, N, U, H, Params, Text]).

private_message(Id, Text, Nick) :-
    answer(Text, Nick, Answer),
    priv_msg(Id, Answer, Nick).

public_message(Id, Text, Nick, Chan) :-
    answer(Text, Nick, Answer),
    priv_msg(Id, Answer, Chan).

answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member(Elt, List),
    string_upper(Elt, UpperElt),
    member(UpperElt, ["HI", "HELLO"]),
    format(atom(Answer), "~w ~w", [Elt, Nick]).

answer(Text, Nick, Answer) :-
    format(atom(Answer), "~w: not understood", [Nick]).
