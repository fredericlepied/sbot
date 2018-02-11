%% -*- prolog -*-

:- module(irc, []).

:- use_module(library(irc_client)).
:- use_module(discuss).

:- initialization
   run.

% assert_handlers/2 is used for asserting event handlers. These event handlers
% must have an arity of 1. The first argument is the Id of the connection. The
% Id of the connection is established as the alias of the thread of the actual
% connection. The second argument is a list of event goals to be be applied to
% incoming server messages. The connection Id _must_ be ground.

% Below, we have use assert_handlers/2 as a directive, but they may not be in
% in your application. If assert_handlers/2 is used in a module file, then you
% must make sure that your module exports the event predicates that you want to
% use with assert_handlers/2.

:- assert_handlers(irc, [irc:output, irc:echo_connected]).


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
    discuss:process_message(Id, Server, Code, Params, Text),
    format("~s: ~s ~s ~w ~s~n", [Id, Server, Code, Params, Text]).

echo_connected(Id-Msg) :-
	% Every incoming message relayed by the server takes the form of a pair ...
	% Id-Msg. The Id is the aliased Id of the connection when the thread for it
	% is created. The Msg part of the pair is the actual message itself.
	Msg = msg(Server, "352", _, _),
	format("Successfully connected to ~s on ~s~n", [Server,Id]).

run :-
    thread_create(connect, _, [detached(true), alias(conn)]).

connect :-
    repeat,
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
    connect('irc.devel.redhat.com', 6667, "", sbot, [host,server,real], ['#testbot']).
