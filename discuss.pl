%% -*- prolog -*-

:- module(discuss, [process_message]).

:- use_module(library(irc_client_utilities)).

process_message(Id, Server, "PRIVMSG", Params, Text) :-
    member('sbot', Params),
    writeln(["privatemsg", Server, Params, Text]).
%    priv_msg(Id, "hello crual world", Param).

process_message(Id, Server, Code, Params, Text) :-
    writeln(["unmanaged", Id, Server, Code, Params, Text]).
