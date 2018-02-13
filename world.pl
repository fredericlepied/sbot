%% -*- prolog -*-

:- module(world, [update_facts/0, update_fact/1, add_fact_updater/1]).

:- dynamic fact, fact_updater.

update_fact(Fact) :-
    asserta(fact(Fact)).

add_fact_updater(Updater) :-
    asserta(fact_updater(Updater)).

update_facts() :-
    retractall(fact(_)),
    fact_updater(Updater),
    Updater,
    fail.

update_facts() :-
    true.

%% world.pl ends here
