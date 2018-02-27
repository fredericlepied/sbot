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
    get_dci_jobs(Jobs),
    member(Component, Jobs.globalStatus),
    store_fact(Gen, dci_component(Component.product_name, Component.topic_name, Component.name)),
    member(Job, Component.jobs),
    store_fact(Gen, dci_job(Component.product_name, Component.topic_name, Component.name, Job.team_name, Job.remoteci_name, Job.status, Job.id, Job.rconfiguration_name)).

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
    get_fact(dci_job(Product, Topic, Component, Team, RemoteCI, JobStatus, JobId, RConf)),
    get_old_fact(dci_job(Product, Topic, Component, Team, RemoteCI, _, OldJobId, RConf)),
    JobId \== OldJobId,
    store_fact(Gen, new_dci_job(Product, Topic, Component, Team, RemoteCI, JobStatus, JobId, RConf)).

deduce_dci_facts(Gen) :-
    Gen \== 1,
    get_fact(dci_job(Product, Topic, Component, Team, RemoteCI, JobStatus, JobId, RConf)),
    not(get_old_fact(dci_job(Product, Topic, Component, Team, RemoteCI, _, _, RConf))),
    store_fact(Gen, new_dci_job(Product, Topic, Component, Team, RemoteCI, JobStatus, JobId, RConf)).

:- add_fact_deducer(dci:deduce_dci_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dci_solver(_) :-
    get_fact(new_dci_component(Product, Topic, NewComponent, OldComponent)),
    format(string(Text), "** [~w][~w] new component uploaded ~w (old was ~w)", [Product, Topic, NewComponent, OldComponent]),
    notification(["dci", Product, Topic, "new_component"], Text).

dci_solver(_) :-
    get_fact(new_dci_component(Product, Topic, NewComponent)),
    format(string(Text), "** [~w][~w] new component uploaded ~w (first one)", [Product, Topic, NewComponent]),
    notification(["dci", Product, Topic, "new_component_uploaded"], Text).

dci_solver(_) :-
    get_fact(dci_component(Product, Topic, NewComponent)),
    get_fact(puddle_info(Topic, _, "latest", Puddle)),
    split_string(NewComponent, " ", "", [_, ComponentPuddle]),
    ComponentPuddle \== Puddle,
    format(string(Text), "** DCI out of sync for ~w. DCI version: ~w | Puddle available: ~w", [Topic, ComponentPuddle, Puddle]),
    notification(["dci", Product, Topic, "out_of_sync"], Text).

dci_solver(_) :-
    get_fact(new_dci_job(Product, Topic, Component, Team, RemoteCI, JobStatus, JobId, RConf)),
    format(string(Text), "** [~w][~w][~w] new job ~w from ~w/~w/~w: ",
           [Product, Topic, Component, JobId, Team, RemoteCI, RConf]),
    colored(JobStatus, Colored),
    notification(["dci", Product, Topic, "new_job"], [Text, Colored]).

colored("success", green("success")).
colored("failure", red("failure")).

:- add_fact_solver(dci:dci_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_dci_jobs(Status) :-
    config(dci_login, DciLogin),
    config(dci_password, DciPassword),
    setup_call_cleanup(
        http_open("https://api.distributed-ci.io/api/v1/global_status",
                  In,
                  [authorization(basic(DciLogin, DciPassword))]),
        json_read_dict(In, Status),
        close(In)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% dci help
dci_answer(List, _, Answer) :-
    member("dci", List),
    member("help", List),
    format(string(Answer), "~n~w~n~w", ["dci products: display the list of available products.", "dci components <Product>: display the list of latest components for a product."]).

% dci products
dci_answer(["dci", "products"], _, Answer) :-
    findall(Product, get_fact(dci_component(Product, _,_)), Products),
    sort(Products, ProductsSorted),
    string_join(", ", ProductsSorted, ProductsText),
    format(string(Answer), "Available Products: ~w", [ProductsText]).

% dci components <Product>
dci_answer(["dci", "components", Product], _, Answer) :-
    findall(Str, dci_answer(Product, Str), Components),
    sort(Components, ComponentsSorted),
    string_join(" ", ComponentsSorted, ComponentsText),
    format(string(Answer), "~w", [ComponentsText]).

dci_answer(Product, Answer) :-
    get_fact(dci_component(Product, Topic, Name)),
    format(atom(Answer), "~n~w ~w", [Topic, Name]).

dci_answer(Answer) :-
    get_fact(dci_component(Product, Topic, Name)),
    format(atom(Answer), "~n~w ~w ~w", [Product, Topic, Name]).

:- add_answerer(dci:dci_answer).

%% dci.pl ends here
