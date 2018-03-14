%% -*- prolog -*-

:- module(githublib,
          [get_github_pr/4, get_github_issue/4, is_github_pr/4, is_github_issue/4
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

get_github_pr(Owner, Project, Pr, Dict) :-
    format(atom(Path), '/repos/~w/~w/pulls/~w', [Owner, Project, Pr]),
    github_json(Path, Dict).

is_github_pr(Url, Owner, Project, PRId) :-
    split_string(Url, "/", "",  [_, _, "github.com", Owner, Project, "pull", PRId]).

get_github_issue(Owner, Project, Issue, Dict) :-
    format(atom(Path), '/repos/~w/~w/issues/~w', [Owner, Project, Issue]),
    github_json(Path, Dict).

is_github_issue(Url, Owner, Project, IssueId) :-
    split_string(Url, "/", "", [_, _, "github.com", Owner, Project, "issues", IssueId]).

%% githublib.pl ends here
