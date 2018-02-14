%% -*- prolog -*-

:- module(world, [get_facts/0, update_fact/2, add_fact_updater/1,
                  add_fact_deducer/1, fact_loop/0, get_fact/1]).

:- dynamic fact, fact_updater, fact_deducer, last_gen.

get_fact(Fact) :-
    last_gen(Gen),
    fact(Gen, Fact).

update_fact(Gen, Fact) :-
    asserta(fact(Gen, Fact)).

add_fact_updater(Updater) :-
    asserta(fact_updater(Updater)).

add_fact_deducer(Deducer) :-
    asserta(fact_deducer(Deducer)).

update_facts(Gen) :-
    fact_updater(Updater),
    call(Updater, Gen),
    fail.

update_facts(_) :-
    true.

deduce_facts(Gen) :-
    fact_deducer(Deducer),
    call(Deducer, Gen),
    fail.

deduce_facts(_) :-
    true.

get_facts(Gen) :-
    format('Updating facts for gen ~w~n', [Gen]),
    update_facts(Gen),
    format('Deducing facts for gen ~w~n', [Gen]),
    deduce_facts(Gen).

get_facts :-
    asserta(last_gen(1)),
    get_facts(1).

fact_loop :-
    asserta(last_gen(1)),
    thread_create(forever, _, [detached(true), alias(bg)]).

forever :-
    repeat,
    between(1, inf, Gen),
    catch(
        get_facts(Gen),
	Err,
	print_message(error, Err)
    ),
    retractall(last_gen(_)),
    asserta(last_gen(Gen)),
    % Keep only the previous generation of facts
    Prev is Gen - 2,
    retractall(fact(Prev,_)),
    sleep(300),
    fail.

%% world.pl ends here
