%% -*- prolog -*-

:- module(discuss, [process_message/5, add_answerer/1, notify/2, notification/2]).

:- use_module(library(irc_client_utilities)).
:- use_module(library(irc_client_parser)).
:- use_module(config).
:- use_module(utils).
:- use_module(world).

:- dynamic answerer/1.

notification(List, Text) :-
    writeln(notification(List, Text)),
    get_longterm_fact(subscription(Sub, Context)),
    sublist(Sub, List),
    notify(Text, Context).

% [irc, "nick", "chan"] -> [irc, "chan"]
% [irc, "chan"] -> [irc, "chan"]
% [] -> []
extended_context([A, _, C], [A, C]).
extended_context([A, B], [A, B]).
extended_context([], []).

process_message(Id, Server, "PRIVMSG", Params, Text) :-
    split_string(Text, " ", "@:.,!", S),
    delete(S, "", CleanList),
    config(irc_nick, IrcNick),
    member(IrcNick, Params),
    prefix_id(Server, Nick, _, _),
    private_message(Id, CleanList, Nick).

process_message(Id, Server, "PRIVMSG", [Param|_], Text) :-
    split_string(Text, " ", "@:.,!", [BotName|S]),
    config(irc_nick, IrcNick),
    IrcNick == BotName,
    prefix_id(Server, Nick, _, _),
    public_message(Id, S, Nick, Param).

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
    encode(Text, Encoded),
    priv_msg(Id, Encoded, Nick).

send_message(Text, [Id, _, Chan]) :-
    encode(Text, Encoded),
    priv_msg(Id, Encoded, Chan).

add_answerer(Pred) :-
    asserta(answerer(Pred)).

answer(TextList, Context, PrefixedAnswer) :-
    answerer(Pred),
    call(Pred, TextList, Context, Answer),
    add_prefix(Context, Answer, PrefixedAnswer),
    !.

% subscribe dlrn ansible
answer(["subscribe", Elt|List], Context, PrefixedAnswer) :-
    extended_context(Context, ExtendedContext),
    store_longterm_fact(subscription([Elt|List], ExtendedContext)),
    string_join(" ", [Elt|List], Text),
    format(string(Answer), "subscribed to ~w", [Text]),
    add_prefix(Context, Answer, PrefixedAnswer),
    !.

% unsubscribe dlrn ansible
answer(["unsubscribe"|List], Context, PrefixedAnswer) :-
    extended_context(Context, ExtendedContext),
    remove_longterm_fact(subscription(List, ExtendedContext)),
    string_join(" ", List, Text),
    format(string(Answer), "unsubscribed from ~w", [Text]),
    add_prefix(Context, Answer, PrefixedAnswer),
    !.

% subscriptions
answer(["subscriptions"], Context, PrefixedAnswer) :-
    extended_context(Context, ExtendedContext),
    findall(Elt,
            (get_longterm_fact(subscription(Sub, ExtendedContext)),
             string_join(" ", Sub, Elt)),
            List),
    string_join("\n", List, Text),
    format(string(Answer), "subscribed to ~w", [Text]),
    add_prefix(Context, Answer, PrefixedAnswer),
    !.

answer(List, [_,Nick|_], Answer) :-
    member(Elt, List),
    string_upper(Elt, UpperElt),
    member(UpperElt, ["HI", "HELLO", "SALUT", "BONJOUR", "HOLA", "HEY", "MORNING"]),
    format(string(Answer), "~w ~w", [Elt, Nick]),
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
    add_prefix(Context,
               ["use ", bold("<module> help"),
                ". Available modules: ", bold(Output), ".\n",
                bold("subscribe <topic> [<subtopic or *>...]\n"),
                bold("unsubscribe <topic> [<subtopic or *>...]\n"),
                bold("subscriptions")], Answer),
    !.

answer(_, Context, Answer) :-
    add_prefix(Context, "not understood. Use 'help' to list what I understand.", Answer).

add_prefix([_, Nick, _], [Text|Rest], [PrefixedText,Text|Rest]) :-
    format(string(PrefixedText), "~w: ", [Nick]),
    !.

add_prefix([_, Nick, _], Text, PrefixedText) :-
    format(string(PrefixedText), "~w: ~w", [Nick, Text]),
    !.

add_prefix(_, Text, Text).

notify(Text, []) :-
    config(irc_channels, [Chan|_]),
    % todo(fl) figure a way to infer a default id
    Context = [irc, Chan],
    add_prefix(Context, Text, PrefixedText),
    send_message(PrefixedText, Context),
    !.

notify(Text, Context) :-
    add_prefix(Context, Text, PrefixedText),
    send_message(PrefixedText, Context).

encode([Entry|Rest], Output) :-
    encode_irc([Entry|Rest], List),
    !,
    string_join("", List, Output).

encode(Text, Text).

encode_irc([], []).

encode_irc([Text|Rest], [Encoded|EncodedRest]) :-
    encode_irc(Text, Encoded),
    encode_irc(Rest, EncodedRest).

encode_irc(white(Text), Encoded) :-
    irc_color("00", Text, Encoded).

encode_irc(black(Text), Encoded) :-
    irc_color("01", Text, Encoded).

encode_irc(blue(Text), Encoded) :-
    irc_color("02", Text, Encoded).

encode_irc(green(Text), Encoded) :-
    irc_color("03", Text, Encoded).

encode_irc(red(Text), Encoded) :-
    irc_color("04", Text, Encoded).

encode_irc(brown(Text), Encoded) :-
    irc_color("05", Text, Encoded).

encode_irc(purple(Text), Encoded) :-
    irc_color("06", Text, Encoded).

encode_irc(orange(Text), Encoded) :-
    irc_color("07", Text, Encoded).

encode_irc(yellow(Text), Encoded) :-
    irc_color("08", Text, Encoded).

encode_irc(teal(Text), Encoded) :-
    irc_color("09", Text, Encoded).

encode_irc(cyan(Text), Encoded) :-
    irc_color("10", Text, Encoded).

encode_irc(light_cyan(Text), Encoded) :-
    irc_color("11", Text, Encoded).

encode_irc(light_blue(Text), Encoded) :-
    irc_color("12", Text, Encoded).

encode_irc(pink(Text), Encoded) :-
    irc_color("13", Text, Encoded).

encode_irc(grey(Text), Encoded) :-
    irc_color("14", Text, Encoded).

encode_irc(light_grey(Text), Encoded) :-
    irc_color("15", Text, Encoded).

encode_irc(bold(Text), Encoded) :-
    irc_color("02", Text, Encoded).

encode_irc(Text, Text).

irc_color(Color, Text, Encoded) :-
    format(string(Encoded), "\x03\~w~w\x03", [Color,Text]).

%% discuss.pl ends here
