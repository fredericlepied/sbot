%% -*- prolog -*-

:- module(discuss, [process_message/5, add_answerer/1, notify/2]).

:- use_module(library(irc_client_utilities)).
:- use_module(library(irc_client_parser)).
:- use_module(config).
:- use_module(utils).

:- dynamic answerer/1.

process_message(Id, Server, "PRIVMSG", Params, Text) :-
    split_string(Text, " ", "@:.,!", S),
    delete(S, "", CleanList),
    config(irc_nick, IrcNick),
    member(IrcNick, Params),
    prefix_id(Server, Nick, _, _),
    private_message(Id, CleanList, Nick).

process_message(Id, Server, "PRIVMSG", [Param|_], Text) :-
    split_string(Text, " ", "@:.,!", S),
    config(irc_nick, IrcNick),
    member(IrcNick, S),
    delete(S, IrcNick, CleanList1),
    delete(CleanList1, "", CleanList),
    prefix_id(Server, Nick, _, _),
    public_message(Id, CleanList, Nick, Param).

process_message(_, _, _, _, _).

private_message(Id, TextList, Nick) :-
    Context = [Id, Nick],
    answer(TextList, Context, Answer),
    send_message(Answer, Context).

public_message(Id, TextList, Nick, Chan) :-
    Context = [Id, Nick, Chan],
    answer(TextList, Context, Answer),
    send_message(Answer, Context).

send_message(Text, [Id, Nick]) :-
    priv_msg(Id, Text, Nick).

send_message(Text, [Id, _, Chan]) :-
    priv_msg(Id, Text, Chan).

add_answerer(Pred) :-
    asserta(answerer(Pred)).

answer(TextList, Context, PrefixedAnswer) :-
    answerer(Pred),
    call(Pred, TextList, Context, Answer),
    add_prefix(Context, Answer, PrefixedAnswer),
    !.

answer(List, [_,Nick|_], Answer) :-
    member(Elt, List),
    string_upper(Elt, UpperElt),
    member(UpperElt, ["HI", "HELLO", "SALUT", "BONJOUR", "HOLA", "HEY"]),
    format(atom(Answer), "~w ~w", [Elt, Nick]),
    !.

answer([Elt|_], Context, Answer) :-
    string_upper(Elt, UpperElt),
    member(UpperElt, ["THX", "THANKS", "THANK"]),
    add_prefix(Context, "you're welcome", Answer),
    !.

answer([Help], Context, Answer) :-
    string_lower(Help, "help"),
    config(modules, List),
    delete(List, irc, Removed),
    delete(Removed, autoupdate, Removed2),
    string_join(", ", Removed2, Output),
    format(atom(Text), "use <module> help. Available modules: ~w", [Output]),
    add_prefix(Context, Text, Answer),
    !.

answer(_, Context, Answer) :-
    add_prefix(Context, "not understood. Use 'help' to list what I understand.", Answer).

add_prefix([_, Nick, _], Text, PrefixedText) :-
    format(string(PrefixedText), "~w: ~w", [Nick, Text]),
    !.

add_prefix(_, Text, Text).

notify(Text, Context) :-
    add_prefix(Context, Text, PrefixedText),
    send_message(PrefixedText, Context).

%% discuss.pl ends here
