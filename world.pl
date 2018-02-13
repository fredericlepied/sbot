%% -*- prolog -*-

:- module(world, [get_facts/0, update_facts/0, update_fact/1,
                  add_fact_updater/1, add_fact_deducer/1]).

:- dynamic fact, fact_updater, fact_deducer.

update_fact(Fact) :-
    asserta(fact(Fact)).

add_fact_updater(Updater) :-
    asserta(fact_updater(Updater)).

add_fact_deducer(Deducer) :-
    asserta(fact_deducer(Deducer)).

update_facts() :-
    retractall(fact(_)),
    fact_updater(Updater),
    Updater,
    fail.

update_facts() :-
    true.

deduce_facts() :-
    fact_deducer(Deducer),
    Deducer,
    fail.

deduce_facts() :-
    true.

get_facts() :-
    update_facts,
    deduce_facts.

%% world.pl ends here
