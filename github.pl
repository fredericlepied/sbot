%% -*- prolog -*-

:- module(github,
          [
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
    update_fact(Gen, github_pr_sha(Owner, Project, Pr, Dict.head.sha)),
    update_fact(Gen, github_pr_html(Owner, Project, Pr, Dict.'_links'.html.href)).

:- add_fact_updater(github:update_github_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deduce_github_facts(Gen) :-
    get_fact(github_pr_sha(Owner, Project, Pr, Sha)),
    get_old_fact(github_pr_sha(Owner, Project, Pr, OldSha)),
    Sha \== OldSha,
    update_fact(Gen, github_updated_pr(Owner, Project, Pr, OldSha)).

:- add_fact_deducer(github:deduce_github_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

github_solver(_) :-
    get_longterm_fact(github_track_pr(Owner, Project, Pr, Requester)),
    get_fact(github_updated_pr(Owner, Project, Pr, Sha)),
    get_fact(github_pr_html(Owner, Project, Pr, Url)),
    config(irc_channels, [Chan|_]),
    format(atom(Text), "~w: Github PR ~w updated (~w)", [Requester, Url, Sha]),
    %% todo(fl) need to remove hardcoded id
    notify(irc, Text, Chan).

:- add_fact_solver(github:github_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% github help
github_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member("github", List),
    member("help", List),
    format(atom(Answer), "~w: github trackpr <owner> <project> <pr>~ngithub trackpr: list all the tracked PR", [Nick]).

% github trackpr ansible ansible 35917
github_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", ["github", "trackpr", Owner, Project, Pr]),
    update_longterm_fact(github_track_pr(Owner, Project, Pr, Nick)),
    last_gen(Gen), update_github_facts(Gen),
    get_fact(github_pr_html(Owner, Project, Pr, Url)),
    format(atom(Answer), "~w: tracking PR ~w", [Nick, Url]).

% github trackpr
github_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", ["github", "trackpr"]),
    findall(Url, get_fact(github_pr_html(_, _, _, Url)), Urls),
    string_join(" ", Urls, TextUrls),
    format(atom(Answer), "~w: tracking PR ~w", [Nick, TextUrls]).

:- add_answerer(github:github_answer).

%% github.pl ends here
