%% -*- prolog -*-

:- module(autoupdate, []).

:- use_module(world).
:- use_module(utils).
:- use_module(discuss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

autoupdate_code(_) :-
    not(cmd("git diff --no-ext-diff --quiet --exit-code", [])),
    make,
    !.

autoupdate_code(_) :-
    cmd("git pull", []),
    make.

:- add_fact_updater(autoupdate:autoupdate_code).

%% autoupdate.pl ends here
