%% -*- prolog -*-

:- begin_tests(test_utils).

:- use_module(utils).

test(dirbase) :-
    utils:dirbase(Dirname, Basename, "a/b/c/d/e"),
    assertion(Basename == "e"),
    assertion(Dirname == "a/b/c/d").

test(string_join) :-
    utils:string_join(" ", ["a", "b", "c"], Res),
    assertion(Res == "a b c").

test(cmd_fail) :-
    not(utils:cmd("fail", [])).

test(cmd_true) :-
    utils:cmd("true", [], []).

test(sublist_empty) :-
    utils:sublist([], []).

test(sublist_simple) :-
    utils:sublist(["a", "b"], ["a", "b", "c"]).

test(sublist_star) :-
    utils:sublist(["a", "*", "c"], ["a", "b", "c"]).

:- end_tests(test_utils).

%% test_utils.pl ends here
