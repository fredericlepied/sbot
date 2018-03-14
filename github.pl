%% -*- prolog -*-

:- module(github,
          [github_answer/3, github_issue/4, github_pullrequest/4
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

update_github_facts(Gen) :-
    config(github, [Owner, Project]),
    github_issues(Owner, Project, Issues),
    member(Issue, Issues),
    store_fact(Gen, github_issue(Owner, Project, Issue.id, Issue.html_url, Issue.title, Issue.state, Issue.user.login)).

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

deduce_github_facts(Gen) :-
    get_fact(github_issue(Owner, Project, Id, Url, Title, "closed", Reporter)),
    get_old_fact(github_issue(_, _, Id, _, _, "open", _)),
    store_fact(Gen, github_closed_issue(Owner, Project, Id, Url, Title, "closed", Reporter)).

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

github_solver(_) :-
    get_fact(github_closed_issue(Owner, Project, _, Url, Title, _, _)),
    format(string(Text), "** [~w/~w] Issue \"~w\" has been closed (~w)", [Owner, Project, Title, Url]),
    format(string(Repo), "~w/~w", [Owner, Project]),
    notification(["github", Repo, "closed_issue"], Text).

:- add_fact_solver(github:github_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% github help
github_answer(["github", "help"], _,
           ["Available commands:\n",
            bold("github trackpr"), ": track the speficied PR.\n",
            bold("github untrackpr"), ": untrack the specified PR.\n",
            "Available notifications\n",
            bold("github <Owner/Project> closed_issue")
           ]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% type predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

github_issue(Url, Owner, Project, IssueId) :-
    split_string(Url, "/", "", [_, _, "github.com", Owner, Project, "issues", IssueId]).

github_pullrequest(Url, Owner, Project, PRId) :-
    split_string(Url, "/", "",  [_, _, "github.com", Owner, Project, "pull", PRId]).

%% github.pl ends here
