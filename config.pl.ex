%% -*- prolog -*-

:- module(config, [config/2]).

%% settings for irc.pl module

config(irc_server, 'irc.freenode.org').
config(irc_port, 6667).
config(irc_nick, "sbot").

% the first chan will get the notifications
config(irc_channels, ["##testbot"]).

%% Configure external modules stored in git for prolog modules or for
%% config.pl. If the modules has a tools subdir it will be added
%% automatically to the PATH env variable.
%config(external_git_module, "file:///home/sbot/repo/sbot-config").

%% add the optional modules you want to activate to the list
config(modules, [dlrn, irc, self, autoupdate, github, puddle, fun, fedora, admin, dci, gerrit, trello]).

%% admin-team members
config(admins, ["admin-1", "admin-2"]).

%% workspace if you want to use something else than ~/public_html
%config(workspace, "/opt/workspace").

%% URL to expose the workspace. Must end with a /
config(workspace_url, "http://mytopdomain.com/~user/").

%% dlrn and github settings
config(dlrn_status_url, ["systemd", "master", "http://38.145.33.116/systemd-master/"]).
config(dlrn_status_url, ["ansible", "devel", "http://38.145.33.116/ansible-devel/"]).

config(github, ["systemd", "systemd"]).
config(github, ["ansible", "ansible"]).

config(gitrepo, ["ansible-distgit", "git://pkgs.fedoraproject.org/rpms/ansible"]).
config(gitrepo, ["systemd-distgit", "git://pkgs.fedoraproject.org/rpms/systemd"]).

%% fedora settings
%config(fedora_user, "user").

%% github account settings
%config(github_token, "ATOKEN").

%% dci settings
% config(dci_login, "login").
% config(dci_password, "password").

%% gerrit account and server
% config(gerrit_access, ["user", "softwarefactory-project.io", 29418]).

%% user is capable of using the event stream 
% config(gerrit_stream, yes).

%% list of projects to follow (can be regexp)
% config(gerrit_projects, ["my-project-.*", "other-project"]).

% trello settings
config(trello_api_secret, "A_SECRET").
config(trello_oauth_token, "atokenexampleuuidnumber").
config(trello_board, "uuid").

%% config.pl ends here
