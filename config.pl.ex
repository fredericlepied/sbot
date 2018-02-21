%% -*- prolog -*-

:- module(config, [config/2]).

%% settings for irc.pl module

config(irc_server, 'irc.freenode.org').
config(irc_port, 6667).
config(irc_nick, "sbot").

% the first chan will get the notifications
config(irc_channels, ["##testbot"]).

%% add the optional modules you want to activate to the list
config(modules, [dlrn, irc, self, autoupdate, github, puddle, fun, fedora]).

%% dlrn and github settings
config(dlrn_status_url, ["systemd", "master", "http://38.145.33.116/systemd-master/"]).
config(dlrn_status_url, ["ansible", "devel", "http://38.145.33.116/ansible-devel/"]).

config(github, ["systemd", "systemd"]).
config(github, ["ansible", "ansible"]).

config(gitrepo, ["ansible-distgit", "git://pkgs.fedoraproject.org/rpms/ansible"]).
config(gitrepo, ["systemd-distgit", "git://pkgs.fedoraproject.org/rpms/systemd"]).

%% fedora settings
%config(fedora_user, "user").

%% config.pl ends here
