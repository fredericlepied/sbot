%% -*- prolog -*-

:- module(rules,
          [git/2, workspace/1, git_workspace/2, git_extract_pr/2,
           get_dlrn_fact/3, dlrn_last_bad/3, dlrn_status/3,
           download_build_last_dlrn_src/2, download_build_dlrn_src/3,
           build_pr/4, distgit_workspace/2
          ]).

:- use_module(kb).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

git(Name, GitUrl) :-
    gitrepo(Name, GitUrl),
    !.

git(Name, GitUrl) :-
    github(Name, Url),
    string_concat(Url, ".git", GitUrl).

workspace(W) :-
    getenv("HOME", Home),
    string_concat(Home, "/workspace", W),
    mkdir(W).

cmd(Fmt, Args) :-
    format(atom(Cmd), Fmt, Args),
    string_concat("+ ", Cmd, StrCmd),
    writeln(StrCmd),
    shell(Cmd).

distgit_workspace(Name, Dir) :-
    string_concat(Name, "-distgit", DistGitName),
    git_workspace(DistGitName, Dir).

git_workspace(Name, Dir) :-
    git(Name, Url),
    workspace(W),
    format(atom(Dir), "~w/~w", [W, Name]),
    git_workspace_aux(Dir, W, Url, Name).

git_workspace_aux(Dir, W, _, Name) :-
    exists_directory(Dir),
    cmd("cd ~w/~w; git fetch", [W, Name]),
    !.

git_workspace_aux(_, W, Url, Name) :-
    cmd("cd ~w; rm -rf ~w; git clone ~w ~w", [W, Name, Url, Name]).

git_extract_pr(Name, Pr) :-
    git_workspace(Name, Dir),
    cmd("cd ~w; git-extract-pr.sh ~w", [Dir, Pr]).

mkdir(D) :-
    exists_directory(D), !.

mkdir(D) :-
    make_directory(D).

get_dlrn_fact(Name, Branch, Info) :-
    get_dlrn_dom(Name, Branch, Dom),
    all_rows(Dom, Rows),
    extract_dlrn_table_data(Rows, Info).

get_dlrn_dom(Name, Branch, DOM) :-
    dlrn_status_url(Name, Branch, BaseUrl),
    string_concat(BaseUrl, "report.html", Url),
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

download_build_last_dlrn_src(Name, Branch) :-
    world:dlrn_info(Name, Branch, [[_,[_,Path,_]]|_]),
    download_dlrn_build_src(Name, Branch, Path).

download_build_dlrn_src(Name, Branch, Path) :-
    dlrn_status_url(Name, Branch, Url),
    workspace(Ws),
    cmd("dlrn_download_srcrpm.sh ~w ~w ~w", [Ws, Url, Path]),
    cmd("dlrn_build_srcrpm.sh ~w ~w", [Ws, Path]).

build_pr(Name, Branch, Path, Pr) :-
    dlrn_status_url(Name, Branch, Url),
    workspace(Ws),
    cmd("dlrn_download_srcrpm.sh ~w ~w ~w", [Ws, Url, Path]),
    git_extract_pr(Name, Pr),
    cmd("dlrn_inject_patch.sh ~w ~w ~w/~w/pr-~w.patch", [Ws, Path, Ws, Name, Pr]),
    cmd("dlrn_build_srcrpm.sh ~w ~w/SRPMS", [Ws, Path]).
    
%% rules.pl ends here
