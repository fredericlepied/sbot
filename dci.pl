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

deduce_dci_facts(Gen) :-
    get_longterm_fact(dci_sync(Topic, Context)),
    workspace("dci", Ws),
    url_workspace("dci", Url),
    format(string(LogPath), "sync/~w/~w", [Topic, Gen]),
    (run_osp_feeder(Topic, Ws, LogPath) -> Status = success; Status = failure),
    format(string(Text), "** [DCI] Syncing of ~w: ", [Topic]),
    format(string(Text2), " (~w~w) ", [Url, LogPath]),
    remove_longterm_fact(dci_sync(Topic, Context)),
    color(Status, Colored),
    notify([Text, Colored, Text2], Context).

color(success, green("success")).
color(failure, red("failure")).

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
dci_answer(List, _, Answer) :-
    member("dci", List),
    member("help", List),
    format(string(Answer), "~n~w~n~w~n~w~n~w",
        ["dci products: display the list of available products.",
         "dci topics <Product>: display the list of topics for a product.",
         "dci components <Product>: display the list of latest components for a product or a topic.",
         "dci sync <Topic>: Trigger the dci feeder for the specified topic."]).

% dci products
dci_answer(["dci", "products"], _, Answer) :-
    findall(Product, get_fact(dci_component(Product, _,_)), Products),
    sort(Products, ProductsSorted),
    string_join(", ", ProductsSorted, ProductsText),
    format(string(Answer), "Available Products: ~w", [ProductsText]).

% dci topics <product>
dci_answer(["dci", "topics", Product], _, Answer) :-
    findall(Product, get_fact(dci_component(Product, _,_)), Products),
    member(Product, Products),
    findall(Topic, get_fact(dci_component(Product, Topic,_)), Topics),
    sort(Topics, TopicsSorted),
    string_join(", ", TopicsSorted, TopicsText),
    format(string(Answer), "Available Topics: ~w", [TopicsText]).

% dci topics <product>: Product does not exist
dci_answer(["dci", "topics", Product], _, Answer) :-
    format(string(Answer), "Product ~w does not exist", [Product]).

% dci components <Product>
dci_answer(["dci", "components", Product], _, Answer) :-
    findall(Product, get_fact(dci_component(Product, _,_)), Products),
    member(Product, Products),
    findall(Str, dci_answer(Product, Str), Components),
    sort(Components, ComponentsSorted),
    string_join(" ", ComponentsSorted, ComponentsText),
    format(string(Answer), "~w", [ComponentsText]).

% dci components <Product>: Product does not exit
dci_answer(["dci", "components", Product], _, Answer) :-
    format(string(Answer), "Product ~w does not exist", [Product]).

% dci sync <Topic>: Old way with no registry
dci_answer(["dci", "sync", Topic], Context, Answer) :-
    member(Topic, ["OSP8", "OSP9", "OSP10", "OSP11"]),
    store_longterm_fact(dci_sync(Topic, Context)),
    format(string(Answer), "Added syncing of ~w to my backlog", [Topic]).

% dci sync <Topic>: Need more work because of containers
dci_answer(["dci", "sync", Topic], _, Answer) :-
    member(Topic, ["OSP12", "OSP13"]),
    format(string(Answer), "Sync for OSP12+ is not yet supported", []).

dci_answer(["dci", "sync", Topic], _, Answer) :-
    format(string(Answer), "Topic ~w does not exist", [Topic]).

dci_answer(Product, Answer) :-
    get_fact(dci_component(Product, Topic, Name)),
    format(atom(Answer), "~n~w ~w", [Topic, Name]).

dci_answer(Answer) :-
    get_fact(dci_component(Product, Topic, Name)),
    format(atom(Answer), "~n~w ~w ~w", [Product, Topic, Name]).

:- add_answerer(dci:dci_answer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_osp_feeder(Topic, Ws, LogPath) :-
    format(string(Output), "~w/~w", [Ws, LogPath]),
    cmd("mkdir -p ~w", [Output]),
    cmd("cd $HOME/dci-feeder-osp && bash osp.sh ~w 2>&1 > ~w/log.txt", [Topic, Output]).

%% dci.pl ends here
