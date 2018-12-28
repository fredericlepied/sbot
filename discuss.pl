%% -*- prolog -*-

:- module(discuss, [process_message/5, add_answerer/1, notify/2, notification/2, notification/3,
                    split_words/2, send_notifications/0, answer/3]).

:- use_module(library(irc_client_utilities)).
:- use_module(library(irc_client_parser)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

:- use_module(config).
:- use_module(gerritlib).
:- use_module(utils).
:- use_module(world).
:- use_module(irc).

:- dynamic answerer/1.

notification(List, Text, Times) :-
    last_gen(Gen),
    0 is Gen mod Times,
    notification(List, Text).

notification(List, Text) :-
    writeln(notification(List, Text)),
    notification_aux(List, Text).

notification_aux(List, Text) :-
    get_longterm_fact(subscription(Sub, Context)),
    sublist(Sub, List),
    store_fact(notify([Text, Context])),
    fail.

notification_aux(_, _).

% [irc, "nick", "chan"] -> [irc, "chan"]
% [irc, "chan"] -> [irc, "chan"]
% [] -> []
extended_context([A, _, C], [A, C]).
extended_context([A, B], [A, B]).
extended_context([], []).

% split a sentence into a list of words handling the '?' that can be
% appended to the last word.
split_words(String, Words) :-
    split_string(String, " ", "@:.,!", List),
    manage_question_mark(List, Words).

manage_question_mark(List, Words) :-
    reverse(List, [First|Reverse]),
    fix_first(First, Fixed),
    append(Fixed, Reverse, FixedList),
    reverse(FixedList, Words).

fix_first("?", ["?"]) :- !.
fix_first(Word, ["?",Fixed]) :-
    string_chars(Word, Chars),
    reverse(Chars, ['?'|Reverse]),
    reverse(Reverse, Straight),
    string_chars(Fixed, Straight),
    !.
fix_first(Word, [Word]).

process_message(Id, Server, "PRIVMSG", Params, Text) :-
    split_words(Text, S),
    delete(S, "", CleanList),
    config(irc_nick, IrcNick),
    member(IrcNick, Params),
    prefix_id(Server, Nick, _, _),
    wait_a_bit,
    private_message(Id, CleanList, Nick).

process_message(Id, Server, "PRIVMSG", [Param|_], Text) :-
    split_words(Text, S),
    config(irc_nick, IrcNick),
    member(IrcNick, S),
    delete(S, IrcNick, CleanList1),
    delete(CleanList1, "", CleanList),
    prefix_id(Server, Nick, _, _),
    wait_a_bit,
    public_message(Id, CleanList, Nick, Param).

process_message(Id, Server, "PRIVMSG", [Chan|_], Text) :-
    split_words(Text, S),
    config(irc_nick, IrcNick),
    not(member(IrcNick, S)),
    prefix_id(Server, Nick, _, _),
    wait_a_bit,
    public_message(Id, S, [Nick,Chan]).

process_message(_, _, _, _, _).

private_message(Id, TextList, Nick) :-
    Context = [Id, Nick],
    update_interactions,
    answer(TextList, Context, Answer),
    send_message(Answer, Context).

% public message directed to the bot
public_message(Id, TextList, Nick, Chan) :-
    Context = [Id, Nick, Chan],
    update_interactions,
    answer(TextList, Context, Answer),
    send_message(Answer, Context).

% public message not directed to the bot
public_message(Id, TextList, [Nick,Chan]) :-
    Context = [Id, Nick, Chan],
    public_answer(Nick, TextList, Context, Answer),
    send_message(Answer, Context).

send_message(Text, [Id, Nick]) :-
    encode(Text, Encoded),
    priv_msg(Id, Encoded, Nick).

send_message(Text, [Id, _, Chan]) :-
    encode(Text, Encoded),
    priv_msg(Id, Encoded, Chan).

add_answerer(Pred) :-
    asserta(answerer(Pred)).

public_answer(Nick, [Text], _, Answer) :-
    string_upper(Text, UpperElt),
    member(UpperElt, ["HI", "HELLO", "SALUT", "BONJOUR", "HOLA", "HEY", "MORNING", "ALOHA", "MATIN", "PLOP", "O/", "\\O/"]),
    format(string(Answer), "~w ~w", [Text, Nick]).
    
public_answer(_, TextList, _, Answer) :-
    extract_first_url(TextList, Url),
    url_to_title(Url, Answer),
    !.

url_to_title(Url, Title) :-
    is_gerrit_review(Url, ReviewId, BaseUrl),
    get_gerrit_review(BaseUrl, ReviewId, Review),
    Title = Review.subject.

url_to_title(Url, Title) :-
    http_open(Url, Stream, []),
    load_html(Stream, Dom, []),
    xpath(Dom, //title(text), Title).

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
    get_latest_nick(Nick, CurrentNick),
    format(string(Answer), "~w ~w", [Elt, CurrentNick]),
    !.

answer([Elt|_], Context, Answer) :-
    string_upper(Elt, UpperElt),
    member(UpperElt, ["THX", "THANKS", "THANK"]),
    add_prefix(Context, "you're welcome", Answer),
    !.

answer([Elt|_], Context, Answer) :-
    string_upper(Elt, UpperElt),
    member(UpperElt, ["GG", "<3"]),
    add_prefix(Context, "Thx", Answer),
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

% forward to NLP module
answer([Topic|Rest], Context, Answer) :-
    Topic \== "lang",
    answer(["lang",Topic|Rest], Context, Answer),
    !.

answer(List, Context, Answer) :-
    string_join(" ", List, Text),
    store_longterm_fact(not_understood_sentence(Text)),
    add_prefix(Context, "not understood. Use 'help' to list what I understand.", Answer).

add_prefix([_, Nick, _], [Text|Rest], [PrefixedText,Text|Rest]) :-
    get_latest_nick(Nick, CurrentNick),
    format(string(PrefixedText), "~w: ", [CurrentNick]),
    !.

add_prefix([_, Nick, _], Text, PrefixedText) :-
    get_latest_nick(Nick, CurrentNick),
    format(string(PrefixedText), "~w: ~w", [CurrentNick, Text]),
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

wait_a_bit :-
    Delay is random_float * 1,
    sleep(Delay).

get_today_date(Today) :-
    get_time(T),
    stamp_date_time(T, date(Y, M, D, _, _, _, _, _, _), 'UTC'),
    format(string(Today), "~w/~w/~w", [Y, M, D]).

update_interactions :-
    get_today_date(Today),
    get_longterm_fact(interaction(Today, Interaction)),
    remove_longterm_fact(interaction(Today, Interaction)),
    NewInteraction is Interaction + 1,
    store_longterm_fact(interaction(Today, NewInteraction)),
    !.

update_interactions :-
    get_today_date(Today),
    store_longterm_fact(interaction(Today, 1)).

send_notifications :-
    setof(Context, get_fact(notify([_, Context])), L),
    map(discuss:send_notifications_by_context, L, _),
    !.

send_notifications.

send_notifications_by_context(Context, _) :-
    length(L, Len),
    findall([Text, Context], get_fact(notify([Text, Context])), L),
    writeln([send_notifications_by_context, Context, Len, L]),
    (Len =< 10 ->
         map(discuss:send_notif, L, _);
     format("Too many notification (~w) for ~w. Not sending anything to avoid flood.", [Len, Context])
    ),
    !.

send_notif([Text, Context], [Text, Context]) :-
    notify(Text, Context).

%% discuss.pl ends here
