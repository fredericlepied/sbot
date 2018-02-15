%% -*- prolog -*-

:- module(config,
          [config/2
          ]).

%% settings for irc.pl module

config(irc_server, 'irc.freenode.org').
config(irc_port, 6667).
config(irc_nick, "sbot").
config(irc_channels, ["##testbot"]).

%% add the optional modules you want to activate to the list
config(modules, [dlrn, irc, self, autoupdate, github, puddle]).

%% config.pl ends here
