%% -*- prolog -*-

:- begin_tests(gerritlib).
:- use_module(gerritlib).

test(is_gerrit_review) :-
    is_gerrit_review("https://softwarefactory-project.io/r/#/c/11299", ReviewId),
    assertion(ReviewId == "11299").

test(is_gerrit_review) :-
    is_gerrit_review("https://softwarefactory-project.io/r/#/c/11299/", ReviewId),
    assertion(ReviewId == "11299").

test(is_gerrit_review) :-
    is_gerrit_review("https://softwarefactory-project.io/r/#/c/11299/3", ReviewId),
    assertion(ReviewId == "11299").

test(is_gerrit_review) :-
    is_gerrit_review("https://review.openstack.org/#/c/553708", ReviewId),
    assertion(ReviewId == "553708").

test(is_gerrit_review) :-
    is_gerrit_review("https://review.openstack.org/#/c/553708/", ReviewId),
    assertion(ReviewId == "553708").

test(is_gerrit_review) :-
    is_gerrit_review("https://review.openstack.org/#/c/553708/3", ReviewId),
    assertion(ReviewId == "553708").

:- end_tests(is_gerrit_review).

%% test_gerritlib.pl ends here
