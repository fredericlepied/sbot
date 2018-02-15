%% -*- prolog -*-

:- module(githublib,
          [github_pr/4, github_prs/3
          ]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

github_json(Path, Dict) :-
    string_concat("https://api.github.com", Path, Url),
    setup_call_cleanup(
        http_open(Url, In, [request_header('Accept'='application/vnd.github.v3+json')]),
        json_read_dict(In, Dict),
        close(In)
    ).

github_pr(Owner, Project, Pr, Dict) :-
    format(atom(Path), '/repos/~w/~w/pulls/~w', [Owner, Project, Pr]),
    github_json(Path, Dict).

github_prs(Owner, Project, Dict) :-
    format(atom(Path), '/repos/~w/~w/pulls', [Owner, Project]),
    github_json(Path, Dict).

%% githublib.pl ends here
