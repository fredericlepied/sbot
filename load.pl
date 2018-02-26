%% -*- prolog -*-

:- use_module(config).
:- use_module(utils).
:- use_module(world).

load_all :-
    config(modules, Modules),
    member(Module, Modules),
    use_module(Module),
    fail.

load_all :-
    true.

:- load_external_modules.
:- load_all.

%% load.pl ends here
