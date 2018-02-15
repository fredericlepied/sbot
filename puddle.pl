%% -*- prolog -*-

:- module(puddle, []).

:- use_module(library(http/http_client)).

:- use_module(kb).
:- use_module(world).
:- use_module(discuss).
:- use_module(config).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_puddle_facts(Gen) :-
    config(puddle, [Product, Version, Url]),
    get_puddle_fact(Version, Url, PuddleInfo),
    update_fact(Gen, puddle_info(Product, Version, Url, PuddleInfo)).

:- add_fact_updater(puddle:update_puddle_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

puddle_status(Product, Version, Url, RepoFile) :-
    get_fact(puddle_info(Product, Version, Url, [[_,_,_,_], RepoFile])).

puddle_summary(Product, Version, Url, Latest, PassedDCI, PassedPhase1, PassedPhase2) :-
    get_fact(puddle_info(Product, Version, Url, [[Latest, PassedDCI, PassedPhase1, PassedPhase2],_])).

get_puddle_fact(Version, Url, PuddleInfo) :-
    get_puddle_name(Url, PassedUrls),
    get_puddle_repo_file(Version, Url, RepoFile),
    PuddleInfo = [PassedUrls, RepoFile].

get_puddle_repo_file(Version, Url, RepoFile) :-
    format(atom(RepoFile), "~w/latest/RH7-RHOS-~w.0.repo", [Url, Version]).

get_puddle_name(Url, PassedUrls) :-
    format(atom(Latest), "~w/latest/COMPOSE_ID", [Url]),
    format(atom(PassedDCI), "~w/passed_dci/COMPOSE_ID", [Url]),
    format(atom(PassedPhase1), "~w/passed_phase1/COMPOSE_ID", [Url]),
    format(atom(PassedPhase2), "~w/passed_phase2/COMPOSE_ID", [Url]),
    http_get(Latest, InLatest, []),
    http_get(PassedDCI, InPassedDCI, []),
    http_get(PassedPhase1, InPassedPhase1, []),
    http_get(PassedPhase2, InPassedPhase2, []),
    PassedUrls = [InLatest, InPassedDCI, InPassedPhase1, InPassedPhase2].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% puddle OSP10 summary
puddle_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member("puddle", List),
    member("summary", List),
    puddle_summary(Product, Version, Url, Latest, PassedDCI, PassedPhase1, PassedPhase2),
    string_concat(Product, Version, OSPVersion),
    member(OSPVersion, List),
    format(atom(Answer), "~w: Summary for ~w (~w)~nLatest: ~w~nPassed DCI: ~w~nPassed Phase1: ~w~nPassed Phase2: ~w~n", [Nick, OSPVersion, Url, Latest, PassedDCI, PassedPhase1, PassedPhase2]).

% puddle OSP10
puddle_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member("puddle", List),
    puddle_status(Product, Version, _, RepoFile),
    string_concat(Product, Version, OSPVersion),
    member(OSPVersion, List),
    format(atom(Answer), "~w:  ~w~w ~w", [Nick, Product, Version, RepoFile]).

% puddle
puddle_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member("puddle", List),
    findall(Str, puddle_answer(Str), Res),
    string_join(', ', Res, Concat),
    format(atom(Answer), "~w: Available puddles: ~w", [Nick, Concat]).

puddle_answer(Answer) :-
    puddle_status(Product, Version, _, _),
    format(atom(Answer), "~w~w", [Product, Version]).

:- add_answerer(puddle:puddle_answer).
