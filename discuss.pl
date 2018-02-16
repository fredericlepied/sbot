%% -*- prolog -*-

:- module(discuss, [process_message/5, add_answerer/1, notify/3]).

:- use_module(library(irc_client_utilities)).
:- use_module(library(irc_client_parser)).
:- use_module(config).
:- use_module(utils).

:- dynamic answerer/1.

process_message(Id, Server, "PRIVMSG", Params, Text) :-
    split_string(Text, " ", "@:.,", S),
    delete(S, "", CleanList),
    config(irc_nick, IrcNick),
    member(IrcNick, Params),
    prefix_id(Server, Nick, _, _),
    private_message(Id, CleanList, Nick).

process_message(Id, Server, "PRIVMSG", [Param|_], Text) :-
    split_string(Text, " ", "@:.,", S),
    config(irc_nick, IrcNick),
    member(IrcNick, S),
    delete(S, IrcNick, CleanList1),
    writeln(CleanList1),
    delete(CleanList1, "", CleanList),
    prefix_id(Server, Nick, _, _),
    public_message(Id, CleanList, Nick, Param).

process_message(_, _, _, _, _).

private_message(Id, TextList, Nick) :-
    answer(TextList, Nick, Answer),
    priv_msg(Id, Answer, Nick).

public_message(Id, TextList, Nick, Chan) :-
    answer(TextList, Nick, Answer),
    priv_msg(Id, Answer, Chan).

add_answerer(Pred) :-
    asserta(answerer(Pred)).

answer(TextList, Nick, Answer) :-
    answerer(Pred),
    call(Pred, TextList, Nick, Answer),
    !.

answer(List, Nick, Answer) :-
    member(Elt, List),
    string_upper(Elt, UpperElt),
    member(UpperElt, ["HI", "HELLO"]),
    format(atom(Answer), "~w ~w", [Elt, Nick]),
    !.

answer([Help], Nick, Answer) :-
    string_lower(Help, "help"),
    config(modules, List),
    delete(List, irc, Removed),
    delete(Removed, autoupdate, Removed2),
    string_join(", ", Removed2, Output),
    format(atom(Answer), "~w: use <module> help. Available modules: ~w", [Nick, Output]),
    !.

answer(_, Nick, Answer) :-
    format(atom(Answer), "~w: not understood. Use 'help' to list what I understand.", [Nick]).

notify(Id, Text, To) :-
    priv_msg(Id, Text, To).

%% discuss.pl ends here
