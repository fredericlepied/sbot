%% -*- prolog -*-

:- module(rules,
          [git/2, workspace/1, git_workspace/2, git_extract_pr/2,
           get_dlrn_fact/3, dlrn_last_bad/3, dlrn_status/3]).

:- use_module(kb).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

git(Name, GitUrl) :-
    github(Name, Url),
    concat(Url, '.git', GitUrl).

workspace(W) :-
    getenv('HOME', Home),
    concat(Home, '/workspace', W),
    mkdir(W).

git_workspace(Name, Dir) :-
    git(Name, Url),
    workspace(W),
    format(atom(Dir), '~w/~w', [W, Name]),
    git_workspace_aux(Dir, W, Url, Name).

git_workspace_aux(Dir, W, _, Name) :-
    exists_directory(Dir),
    format(atom(Cmd), 'cd ~w/~w; git fetch', [W, Name]),
    shell(Cmd),
    !.

git_workspace_aux(_, W, Url, Name) :-
    format(atom(Cmd), 'cd ~w; rm -rf ~w; git clone ~w ~w', [W, Name, Url, Name]),
    shell(Cmd).

git_extract_pr(Name, Pr) :-
    workspace(W),
    format(atom(Cmd), 'cd ~w/~w; git-extract-pr.sh ~w', [W, Name, Pr]),
    shell(Cmd).

mkdir(D) :-
    exists_directory(D), !.

mkdir(D) :-
    make_directory(D).

get_dlrn_fact(Name, Branch, Info) :-
    get_dlrn_dom(Name, Branch, Dom),
    all_rows(Dom, Rows),
    extract_dlrn_table_data(Rows, Info).

get_dlrn_dom(Name, Branch, DOM) :-
    dlrn_status_url(Name, Branch, Url),
    http_open(Url, In, []),
    call_cleanup(
            load_html(In, DOM, []),
            close(In)).

all_rows(Dom, Rows) :-
    findall(Row, xpath(Dom, //tr, Row), [_|Rows]).

extract_dlrn_table_data([Row|Rows], [Data|Rest]) :-
    extract_dlrn_row_data(Row, Data),
    !,
    extract_dlrn_table_data(Rows, Rest).

extract_dlrn_table_data([], []).

extract_dlrn_row_data(Row, [success, Urls]) :-
    xpath:element_attributes(Row, [class=success]),
    findall(Url, xpath(Row, //a(@href), Url), Urls).

extract_dlrn_row_data(Row, [failure, Urls]) :-
    xpath:element_attributes(Row, []),
    findall(Url, xpath(Row, //a(@href), Url), Urls).

dlrn_last_bad(Name, Branch, Sha1) :-
    world:dlrn_info(Name, Branch, Info),
    dlrn_find_last_bad_aux(Info, Sha1).

dlrn_find_last_bad_aux([[failure,Fail],[success, _]|_], Sha1) :-
    basename(Fail, Sha1),
    !.

dlrn_find_last_bad_aux([[failure, _]|Rest], B) :-
    dlrn_find_last_bad_aux(Rest, B).

basename(Name, Base) :-
    split_string(Name, "/", "", List),
    last(List, Base).

dlrn_status(Name, Branch, Status) :-
    world:dlrn_info(Name, Branch, [[Status,_]|_]).

%% rules.pl ends here
