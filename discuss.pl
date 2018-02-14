%% -*- prolog -*-

:- module(discuss, [process_message/5, add_answerer/1]).

:- use_module(library(irc_client_utilities)).
:- use_module(library(irc_client_parser)).

:- dynamic answerer.

process_message(Id, Server, "PRIVMSG", Params, Text) :-
    member("sbot", Params),
    prefix_id(Server, Nick, _, _),
    private_message(Id, Text, Nick).
    
process_message(Id, Server, "PRIVMSG", [Param|_], Text) :-
    split_string(Text, " ", "@:", S),
    member("sbot", S),
    prefix_id(Server, Nick, _, _),
    public_message(Id, Text, Nick, Param).

process_message(_, _, _, _, _).

private_message(Id, Text, Nick) :-
    answer(Text, Nick, Answer),
    priv_msg(Id, Answer, Nick).

public_message(Id, Text, Nick, Chan) :-
    answer(Text, Nick, Answer),
    priv_msg(Id, Answer, Chan).

add_answerer(Pred) :-
    asserta(answerer(Pred)).

answer(Text, Nick, Answer) :-
    answerer(Pred),
    call(Pred, Text, Nick, Answer),
    !.

answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member(Elt, List),
    string_upper(Elt, UpperElt),
    member(UpperElt, ["HI", "HELLO"]),
    format(atom(Answer), "~w ~w", [Elt, Nick]),
    !.

answer(_, Nick, Answer) :-
    format(atom(Answer), "~w: not understood", [Nick]).

%% discuss.pl ends here
