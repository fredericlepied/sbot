%% -*- prolog -*-

:- module(world, [get_facts/0, update_fact/2, add_fact_updater/1,
                  add_fact_deducer/1, add_fact_solver/1, fact_loop/0,
                  get_fact/1, get_old_fact/1, update_longterm_fact/1,
                  get_longterm_fact/1, last_gen/1, remove_longterm_fact/1
                 ]).

:- dynamic fact/2, fact_updater/1, fact_deducer/1, fact_solver/1, last_gen/1.

get_fact(Fact) :-
    last_gen(Gen),
    fact(Gen, Fact).

get_old_fact(Fact) :-
    last_gen(Gen),
    OldGen is Gen - 1,
    fact(OldGen, Fact).

get_longterm_fact(Fact) :-
    fact(-1, Fact).

update_fact(Gen, Fact) :-
    fact(Gen, Fact),
    !.

update_fact(Gen, Fact) :-
    asserta(fact(Gen, Fact)).

update_longterm_fact(Fact) :-
    fact(-1, Fact),
    !.

update_longterm_fact(Fact) :-
    asserta(fact(-1, Fact)).

remove_longterm_fact(Fact) :-
    retractall(fact(-1, Fact)).

add_fact_updater(Updater) :-
    fact_updater(Updater),
    !.

add_fact_updater(Updater) :-
    asserta(fact_updater(Updater)).

add_fact_deducer(Deducer) :-
    fact_deducer(Deducer),
    !.

add_fact_deducer(Deducer) :-
    fact_deducer(Deducer),
    !.

add_fact_deducer(Deducer) :-
    asserta(fact_deducer(Deducer)).

add_fact_solver(Solver) :-
    fact_solver(Solver),
    !.

add_fact_solver(Solver) :-
    asserta(fact_solver(Solver)).

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

solve_facts(Gen) :-
    fact_solver(Solver),
    call(Solver, Gen),
    fail.

solve_facts(_) :-
    true.

get_facts(Gen) :-
    format('Updating facts for gen ~w~n', [Gen]),
    update_facts(Gen),
    format('Deducing facts for gen ~w~n', [Gen]),
    deduce_facts(Gen),
    format('Solving facts for gen ~w~n', [Gen]),
    solve_facts(Gen).

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
