%% -*- prolog -*-

:- module(gerritlib,
          [get_gerrit_review/3, is_gerrit_review/3
          ]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(readutil)).

:- use_module(config).
:- use_module(utils).

get_gerrit_review(BaseUrl, ReviewId, Review) :-
    gerrit_rest_get(BaseUrl, "changes", ReviewId, Review).

% Vanilla Gerrit: https://DOMAIN/#/c/553708
is_gerrit_review(Url, ReviewId, BaseUrl) :-
    split_string(Url, "/", "",  [Scheme, _, Host, "#", "c", ReviewId|_]),
    format(string(BaseUrl), "~w//~w", [Scheme, Host]).

% SoftwareFactory Gerrit: https://DOMAIN/r/#/c/553708
is_gerrit_review(Url, ReviewId, BaseUrl) :-
    split_string(Url, "/", "",  [Scheme, _, Host, "r", "#", "c", ReviewId|_]),
    format(string(BaseUrl), "~w//~w/r", [Scheme, Host]).

% generic gerrit GET REST API call
gerrit_rest_get(BaseUrl, Path, Args, Result) :-
    format(string(Url), "~w/~w/~w", [BaseUrl, Path, Args]),
    setup_call_cleanup(
        http_open(Url, In, []),
        read_gerrit_json(In, Result),
        close(In)
    ).

read_gerrit_json(In, Result) :-
    % remove stupid header line
    read_line_to_codes(In, _),
    % read real json
    json_read_dict(In, Result).
    
%% gerritlib.pl ends here
