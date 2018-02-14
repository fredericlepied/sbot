%% -*- prolog -*-

:- use_module(kb).
:- use_module(world).
:- use_module(config).

load_all :-
    config(modules, Modules),
    member(Module, Modules),
    use_module(Module),
    fail.

load_all :-
    true.

:- load_all.

%% load.pl ends here
