%% -*- prolog -*-

:- module(dci, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
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
    store_fact(Gen, new_dci_component(Product, Topic, NewComponent)),
    sync_puddle(Product, Topic, Reply),
    store_midterm_fact(dci_sync_job(Reply.event_id, Reply.message)).

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
    get_puddle_from_component(NewComponent, ComponentPuddle),
    ComponentPuddle \== Puddle,
    not(get_fact(puddle_unhealthy(Topic, _, Puddle, _))),
    format(string(Text), "** DCI out of sync for ~w. DCI version: ~w | Puddle available: ~w", [Topic, ComponentPuddle, Puddle]),
    % notify only every 60 mn (12 x 5) to avoid flooding the chan every 5 mn
    notification(["dci", Product, Topic, "out_of_sync"], Text, 12).

dci_solver(_) :-
    get_fact(new_dci_job(Product, Topic, Component, Team, RemoteCI, JobStatus, JobId, RConf)),
    format(string(Text), "** [~w][~w][~w] new job ~w from ~w/~w/~w: ",
           [Product, Topic, Component, JobId, Team, RemoteCI, RConf]),
    colored(JobStatus, Colored),
    notification(["dci", Product, Topic, "new_job"], [Text, Colored]).

dci_solver(_) :-
    get_midterm_fact(dci_sync_job(EventId, Message)),
    get_sync_job_status(EventId, Status),
    split_string(Message, " ", "", [_, _, Item, _, _, _]),
    format(string(Text), "Status of ~w import: ~w (http://feeder.distributed-ci.io/logs/~w)", [Item, Status, EventId]),
    notification(["dci", "sync_job"], [Text]),
    remove_midterm_fact(dci_sync_job(EventId, Message)).

% OSP Puddles: RH7-RHOS-14.0 2018-06-13.2
get_puddle_from_component(Component, ComponentPuddle) :-
    split_string(Component, " ", "", [_, ComponentPuddle]),
    !.

% RHEL Puddles: RHEL-7.6-20180617.n.0
get_puddle_from_component(Component, ComponentPuddle) :-
    split_string(Component, "-", "", [_, _, ComponentPuddle]),
    !.

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

sync_puddle(Product, Topic, Reply) :-
    format(string(Url), "http://feeder.distributed-ci.io/~w/~w", [Product, Topic]),
    setup_call_cleanup(
        http_open(Url, In, [status_code(_), method(post)]),
        json_read_dict(In, Reply),
        close(In)
    ).

get_sync_job_status(Id, Status) :-
    format(string(Url), "http://feeder.distributed-ci.io/logs/~s", [Id]),
    setup_call_cleanup(
        http_open(Url, In, []),
        read_string(In, _, Reply),
        close(In)
    ),
    split_string(Reply, "\n", "", List),
    reverse(List, [_, StatusLine|_]),
    split_string(StatusLine, " ", "", LastLineWords),
    reverse(LastLineWords, [Status|_]),
    member(Status, ["SUCCESS", "FAILURE", "UNCHANGED"]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% dci help
dci_answer(["dci", "help"], _,
           ["Available commands:\n",
            bold("dci products"), ": display the list of available products.\n",
            bold("dci components <product>"), ": display the list of latest components for a product.\n",
            bold("dci sync <product> <topic>"), ": synchronize the specified topic.\n",
            "Available notifications\n",
            bold("dci <product> <topic> new_component\n"),
            bold("dci <product> <topic> out_of_sync\n"),
            bold("dci <product> <topic> new_job\n"),
            bold("dci sync_job")
           ]).

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

% dci sync <Product> <Topic>
dci_answer(["dci", "sync", Product, Topic], _, Answer) :-
    sync_puddle(Product, Topic, Reply),
    store_midterm_fact(dci_sync_job(Reply.event_id, Reply.message)),
    format(string(Answer), "~w (http://feeder.distributed-ci.io/logs/~w)", [Reply.message, Reply.event_id]).

dci_answer(Product, Answer) :-
    get_fact(dci_component(Product, Topic, Name)),
    format(string(Answer), "~n~w ~w", [Topic, Name]).

dci_answer(Answer) :-
    get_fact(dci_component(Product, Topic, Name)),
    format(string(Answer), "~n~w ~w ~w", [Product, Topic, Name]).

:- add_answerer(dci:dci_answer).

%% dci.pl ends here
