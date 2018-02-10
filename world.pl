%% -*- prolog -*-

:- module(world, [update_facts/0]).

:- use_module(rules).
:- use_module(kb).

:- dynamic dlrn_info.

update_facts() :-
    update_dlrn_facts.

update_dlrn_facts() :-
    retractall(dlrn_info/3),
    dlrn_status_url(Name, Branch, _),
    get_dlrn_fact(Name, Branch, Info),
    asserta(dlrn_info(Name, Branch, Info)),
    fail.

update_dlrn_facts() :-
    true.

%% world.pl ends here
