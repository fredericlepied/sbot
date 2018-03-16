%% -*- prolog -*-

:- module(prometheus, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- use_module(world).
:- use_module(discuss).
:- use_module(config).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_prometheus_facts(Gen) :-
    get_prometheus_alerts(Alerts),
    member(Alert, Alerts.data),
    store_fact(Gen, prometheus_alert(Alert.fingerprint, Alert)).

:- add_fact_updater(prometheus:update_prometheus_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% alert
prometheus_solver(_) :-
    get_fact(prometheus_alert(_, Alert)),
    format(string(Text), "** [CURRENT][~w][~w] ~w", [Alert.labels.severity, Alert.labels.instance, Alert.annotations.summary]),
    % notify only every 60 mn (12 x 5) to avoid flooding the chan every 5 mn
    notification(["prometheus", "current_alert"], Text, 12).

% new alert
prometheus_solver(_) :-
    get_fact(prometheus_alert(Fingerprint, Alert)),
    not(get_old_fact(prometheus_alert(Fingerprint, _))),
    format(string(Text), "** [NEW][~w][~w] ~w", [Alert.labels.severity, Alert.labels.instance, Alert.annotations.summary]),
    notification(["prometheus", "new_alert"], Text).

% solved alert
prometheus_solver(_) :-
    get_old_fact(prometheus_alert(Fingerprint, Alert)),
    not(get_fact(prometheus_alert(Fingerprint, _))),
    format(string(Text), "** [SOLVED][~w][~w] ~w", [Alert.labels.severity, Alert.labels.instance, Alert.annotations.summary]),
    notification(["prometheus", "solved_alert"], Text).

:- add_fact_solver(prometheus:prometheus_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_prometheus_alerts(Alerts) :-
    config(prometheus_alertmanager, AlertManager),
    format(string(AlertManagerApiUrl), "~w/api/v1/alerts", [AlertManager]),
    setup_call_cleanup(
        http_open(AlertManagerApiUrl, In, []),
        json_read_dict(In, Alerts),
        close(In)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prometheus help
prometheus_answer(["prometheus", "help"], _,
           ["Available commands:\n",
            bold("prometheus alerts"), ": display all the alerts.\n",
            bold("prometheus alert info <fingerprint>"), ": display info about specific alert.\n",
            "Available notifications\n",
            bold("prometheus current_alert\n"),
            bold("prometheus new_alert\n"),
            bold("prometheus solved_alert")
           ]).

% prometheus alerts
prometheus_answer(["prometheus", "alerts"], _, Answer) :-
    findall(Alert, prometheus_answer(Alert), Alerts),
    string_join("\n", Alerts, AlertsText),
    format(string(Answer), "\n~w", [AlertsText]).

% prometheus alerts info fingerprint
prometheus_answer(["prometheus", "alert", "info", Fingerprint], _, Answer) :-
    get_fact(prometheus_alert(Fingerprint, Alert)),
    format(string(Answer), "[~w][~w][~w]\nDescription: ~w\nState: ~w\nActive since: ~w", [Alert.labels.severity, Alert.labels.instance, Alert.labels.alertname, Alert.annotations.description, Alert.status.state, Alert.startsAt]).

prometheus_answer(["prometheus", "alert", "info", Fingerprint], _, Answer) :-
    format(string(Answer), "Alert with fingerprint ~w: not found", [Fingerprint]).

prometheus_answer(Answer) :-
    get_fact(prometheus_alert(_, Alert)),
    format(string(Answer), "[~w][~w][~w] ~w", [Alert.fingerprint, Alert.labels.severity, Alert.labels.instance, Alert.annotations.summary]).

:- add_answerer(prometheus:prometheus_answer).

%% prometheus.pl ends here
