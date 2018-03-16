%% -*- prolog -*-

:- module(gerritlib,
          [get_gerrit_review/2, is_gerrit_review/2
          ]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(utils).
:- use_module(gerrit).

get_gerrit_review(ReviewId, Review) :-
    format(string(Query), "~w", ReviewId),
    gerrit_query(Query, [Review|_]).

% Vanilla Gerrit: https://DOMAIN/#/c/553708
is_gerrit_review(Url, ReviewId) :-
    split_string(Url, "/", "",  [_, _, _, "#", "c", ReviewId|_]).

% SoftwareFactory Gerrit: https://DOMAIN/r/#/c/553708
is_gerrit_review(Url, ReviewId) :-
    split_string(Url, "/", "",  [_, _, _, "r", "#", "c", ReviewId|_]).

%% gerritlib.pl ends here
