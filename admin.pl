%% -*- prolog -*-

:- module(admin,
          [
          ]).

:- use_module(discuss).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

admin_answer(List, [_,Nick|_], Answer) :-
    config(admins, Admins),
    member("admin", List),
    member("help", List),
    member(Nick, Admins),
    format(string(Answer), "~n~w", ["kill: Kill Botsito"]).

admin_answer(List, _, Answer) :-
    member("admin", List),
    member("help", List),
    format(string(Answer), "~w", ["Not allowed"]).

admin_answer(["kill"], [_,Nick|_], _) :-
    config(admins, Admins),
    member(Nick, Admins),
    halt.

admin_answer(["kill"], _, Answer) :-
    format(string(Answer), "~w", ["Not allowed"]).

admin_answer(["admins"], _, Answer) :-
    config(admins, Admins),
    string_join(", ", Admins, Answer).

:- add_answerer(admin:admin_answer).

%% admin.pl ends here
