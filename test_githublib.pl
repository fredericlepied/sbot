%% -*- prolog -*-

:- begin_tests(githublib).
:- use_module(githublib).

test(is_github_issue) :-
    is_github_issue("https://github.com/redhat-cip/dci-control-server/issues/58", Owner, Project, IssueId),
    assertion(Owner == "redhat-cip"),
    assertion(Project == "dci-control-server"),
    assertion(IssueId == "58").

test(is_github_pr) :-
    is_github_pr("https://github.com/ansible/ansible/pull/37395", Owner, Project, PRId),
    assertion(Owner == "ansible"),
    assertion(Project == "ansible"),
    assertion(PRId == "37395").

:- end_tests(githublib).

%% test_githublib.pl ends here
