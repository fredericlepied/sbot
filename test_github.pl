%% -*- prolog -*-

:- begin_tests(github).
:- use_module(github).

test(github_issue) :-
    github_issue("https://github.com/redhat-cip/dci-control-server/issues/58", Owner, Project, IssueId),
    assertion(Owner == "redhat-cip"),
    assertion(Project == "dci-control-server"),
    assertion(IssueId == "58").

test(github_pullrequest) :-
    github_pullrequest("https://github.com/ansible/ansible/pull/37395", Owner, Project, PRId),
    assertion(Owner == "ansible"),
    assertion(Project == "ansible"),
    assertion(PRId == "37395").

:- end_tests(github).

%% test_github.pl ends here
