%% -*- prolog -*-

:- module(autoupdate, []).

:- use_module(world).
:- use_module(utils).
:- use_module(discuss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% working directory is dirty
autoupdate_code(_) :-
    not(cmd("git diff --no-ext-diff --quiet --exit-code", [])),
    make,
    !.

% working directory is clean, so update from remote with support for
% local commits not pushed
autoupdate_code(_) :-
    cmd("git fetch origin; git rebase origin/master", []),
    make.

:- add_fact_updater(autoupdate:autoupdate_code).

%% autoupdate.pl ends here
