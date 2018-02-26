%% -*- prolog -*-

:- module(autoupdate, []).

:- use_module(world).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

autoupdate_updater(_) :-
    autoupdate_code("."),
    findall(GitUrl, (config(external_git_module, GitUrl),
                     dirbase(_, Base, GitUrl),
                     format(string(Dir), "modules/~w", [Base]),
                     autoupdate_code(Dir)), _).

% working directory is dirty
autoupdate_code(Dir) :-
    not(cmd("cd ~w && git diff --no-ext-diff --quiet --exit-code", [Dir])),
    make,
    !.

% working directory is clean, so update from remote with support for
% local commits not pushed
autoupdate_code(Dir) :-
    cmd("cd ~w && git fetch origin; git rebase origin/master", [Dir]),
    make.

:- add_fact_updater(autoupdate:autoupdate_updater).

%% autoupdate.pl ends here
