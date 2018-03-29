%% -*- prolog -*-

:- module(irc, [get_latest_nick/2]).

:- dynamic nick_rename/2.

:- use_module(library(irc_client)).
:- use_module(discuss).
:- use_module(config).
:- use_module(world).

:- initialization
   run.

% The next two predicates below (output/1 and echo_connected/1) are the actual
% implementations of the event goals that process the incoming server messages.
% As mentioned in the README of this pack, this is a very low level protocol
% library. Hence, not even outputting IRC messages is included by default!!!
% However, this is easy enough to implement, as shown by the output/1 predicate
% below. It is a mere 3 line event handler. The other handler, echo_connected/1,
% simply echoes to the user terminal that a successful connection to a server
% has been established when it reaches IRC message code 352.

output(Id-Msg) :-
    % This is the basic format of an incoming server message. It is a compound
    % term of the format: msg(Server, Code, Params, Text), or in other words,
    % msg(string, string, list(string), string)
    % The third argument is a variable list of strings that represent specific
    % IRC parameters
    Msg = msg(Server, Code, Params, Text),
    discuss:process_message(Id, Server, Code, Params, Text).

nick_change(_-Msg) :-
    Msg = msg(Server, "NICK", _, Text),
    split_string(Server, "!", "", [OldNick|_]),
    string_codes(NewNick, Text),
    record_nick_change(OldNick, NewNick).

echo_connected(Id-Msg) :-
    % Every incoming message relayed by the server takes the form of a pair ...
    % Id-Msg. The Id is the aliased Id of the connection when the thread for it
    % is created. The Msg part of the pair is the actual message itself.
    Msg = msg(Server, "352", _, _),
    format("Successfully connected to ~s on ~s~n", [Server,Id]).

record_nick_change(_, NewNick) :-
    % In order to avoid infinite loop, n -> n1, n1 -> n2, n2 -> n it is ensured
    % that if a fact already exist with new nick it is first removed. Leading to
    % n1 -> n2, n2 -> n
    get_midterm_fact(nick_rename(NewNick, _)),
    remove_midterm_fact(nick_rename(NewNick, _)),
    !.

record_nick_change(OldNick, NewNick) :-
    % Store every change name n -> n1, n1 -> n2
    store_midterm_fact(nick_rename(OldNick, NewNick)).

get_latest_nick(OldNick, CurrentNick) :-
    get_midterm_fact(nick_rename(OldNick, X)),
    get_latest_nick(X, CurrentNick),
    !.

get_latest_nick(Nick, Nick).

run :-
    thread_create(connect, _, [detached(true), alias(conn)]).

connect :-
    repeat,
    assert_handlers(irc, [irc:output, irc:nick_change, irc:echo_connected]),
    catch(
	thread_create(join_channels, _, [alias(irc), at_exit(disconnect(irc))]),
	Err,
	print_message(error, Err)
    ),
    thread_join(irc, _),
    writeln("Connection lost, attempting to reconnect ..."),
    sleep(120),
    fail.

join_channels :-
    config(irc_server, IrcServer),
    config(irc_port, IrcPort),
    config(irc_nick, IrcNick),
    config(irc_channels, IrcChannels),
    connect(IrcServer, IrcPort, "", IrcNick, [host,server,real], IrcChannels).
