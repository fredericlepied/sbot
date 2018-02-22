%% -*- prolog -*-

:- module(dci, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- use_module(world).
:- use_module(discuss).
:- use_module(config).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_dci_facts(Gen) :-
    get_dci_components(Components),
    member(Component, Components.components),
    store_fact(Gen, dci_component(Component.product_name, Component.topic_name, Component.name)).

:- add_fact_updater(dci:update_dci_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deduce_dci_facts(Gen) :-
    get_fact(dci_component(Product, Topic, NewComponent)),
    get_old_fact(dci_component(Product, Topic, OldComponent)),
    NewComponent \== OldComponent,
    store_fact(Gen, new_dci_component(Product, Topic, NewComponent, OldComponent)).

deduce_dci_facts(Gen) :-
    Gen \== 1,
    get_fact(dci_component(Product, Topic, NewComponent)),
    not(get_old_fact(dci_component(Product, Topic, _))),
    store_fact(Gen, new_dci_component(Product, Topic, NewComponent)).

:- add_fact_deducer(dci:deduce_dci_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dci_solver(_) :-
    get_fact(new_dci_component(Product, Topic, NewComponent, OldComponent)),
    format(string(Text), "** [~w][~w] new component uploaded ~w (old was ~w)", [Product, Topic, NewComponent, OldComponent]),
    notify(Text, []).

dci_solver(_) :-
    get_fact(new_dci_component(Product, Topic, NewComponent)),
    format(string(Text), "** [~w][~w] new component uploaded ~w (first one)", [Product, Topic, NewComponent]),
    notify(Text, []).

:- add_fact_solver(dci:dci_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_dci_components(Components) :-
    config(dci_login, DciLogin),
    config(dci_password, DciPassword),
    setup_call_cleanup(
        http_open("https://api.distributed-ci.io/api/v1/components/latest",
                  In,
                  [authorization(basic(DciLogin, DciPassword))]),
        json_read_dict(In, Components),
        close(In)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% dci help
dci_answer(List, _, "dci components: display the list of available components.") :-
    member("dci", List),
    member("help", List).

% dci components
dci_answer(["dci", "components"], _, Answer) :-
    findall(Str, dci_answer(Str), Component),
    string_join(" ", Component, ComponentText),
    format(string(Answer), "~w", [ComponentText]).

% dci components <Prodduct>
dci_answer(["dci", "components", Product], _, Answer) :-
    findall(Str, dci_answer(Product, Str), Component),
    string_join(" ", Component, ComponentText),
    format(string(Answer), "~w", [ComponentText]).

dci_answer(Product, Answer) :-
    get_fact(dci_component(Product, Topic, Name)),
    format(atom(Answer), "~n~w ~w", [Topic, Name]).

dci_answer(Answer) :-
    get_fact(dci_component(Product, Topic, Name)),
    format(atom(Answer), "~n~w ~w ~w", [Product, Topic, Name]).

:- add_answerer(dci:dci_answer).

%% dci.pl ends here
