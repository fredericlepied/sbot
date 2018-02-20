%% -*- prolog -*-

:- module(github,
          [github_answer/3
          ]).

:- use_module(config).
:- use_module(discuss).
:- use_module(githublib).
:- use_module(utils).
:- use_module(world).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_github_facts(Gen) :-
    get_longterm_fact(github_track_pr(Owner, Project, Pr, _)),
    github_pr(Owner, Project, Pr, Dict),
    store_fact(Gen, github_pr_sha(Owner, Project, Pr, Dict.head.sha)),
    store_fact(Gen, github_pr_html(Owner, Project, Pr, Dict.'_links'.html.href)),
    merge_status(Dict.merged, Status),
    store_fact(Gen, github_pr_merged(Owner, Project, Pr, Status)).

merge_status(false, no).
merge_status(true, yes).

:- add_fact_updater(github:update_github_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deduce_github_facts(Gen) :-
    get_fact(github_pr_sha(Owner, Project, Pr, Sha)),
    get_old_fact(github_pr_sha(Owner, Project, Pr, OldSha)),
    Sha \== OldSha,
    store_fact(Gen, github_updated_pr(Owner, Project, Pr, Sha, OldSha)).

:- add_fact_deducer(github:deduce_github_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

github_solver(_) :-
    get_longterm_fact(github_track_pr(Owner, Project, Pr, Context)),
    get_fact(github_updated_pr(Owner, Project, Pr, Sha, _)),
    get_fact(github_pr_html(Owner, Project, Pr, Url)),
    format(string(Text), "Github PR ~w updated (~w)", [Url, Sha]),
    notify(Text, Context).

github_solver(_) :-
    get_longterm_fact(github_track_pr(Owner, Project, Pr, Context)),
    get_fact(github_pr_merged(Owner, Project, Pr, yes)),
    get_old_fact(github_pr_merged(Owner, Project, Pr, no)),
    get_fact(github_pr_html(Owner, Project, Pr, Url)),
    format(string(Text), "Github PR ~w merged", [Url]),
    notify(Text, Context).

:- add_fact_solver(github:github_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% github help
github_answer(List, _, "github trackpr <owner> <project> <pr>\ngithub untrackpr <owner> <project> <pr>\ngithub trackpr: list all the tracked PR") :-
    member("github", List),
    member("help", List).

% github trackpr ansible ansible 35917
github_answer(["github", "trackpr", Owner, Project, Pr], Context, Answer) :-
    store_longterm_fact(github_track_pr(Owner, Project, Pr, Context)),
    last_gen(Gen), update_github_facts(Gen),
    get_fact(github_pr_html(Owner, Project, Pr, Url)),
    format(string(Answer), "tracking PR ~w", [Url]).

% github untrackpr ansible ansible 35917
github_answer(["github", "untrackpr", Owner, Project, Pr], Context, Answer) :-
    remove_longterm_fact(github_track_pr(Owner, Project, Pr, Context)),
    get_fact(github_pr_html(Owner, Project, Pr, Url)),
    format(string(Answer), "not tracking PR ~w anymore", [Url]).

% github trackpr
github_answer(["github", "trackpr"], _, Answer) :-
    findall(Url, get_fact(github_pr_html(_, _, _, Url)), Urls),
    string_join(" ", Urls, TextUrls),
    format(string(Answer), "tracking PR ~w", [TextUrls]).

:- add_answerer(github:github_answer).

%% github.pl ends here
