%% -*- prolog -*-

:- module(dlrn,
          [git/2, workspace/1, git_workspace/2, git_extract_pr/2,
           get_dlrn_fact/3, dlrn_last_bad/3, dlrn_status/3,
           download_build_last_dlrn_src/2, download_build_dlrn_src/3,
           build_pr/4, distgit_workspace/2, publish_patch/3
          ]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

:- use_module(kb).
:- use_module(world).
:- use_module(discuss).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_dlrn_facts(Gen) :-
    dlrn_status_url(Name, Branch, _),
    get_dlrn_fact(Name, Branch, Info),
    update_fact(Gen, dlrn_info(Name, Branch, Info)).

:- add_fact_updater(dlrn:update_dlrn_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deduce_dlrn_facts(Gen) :-
    dlrn_status(Name, Branch, failure),
    update_fact(Gen, dlrn_problem(Name, Branch)).

:- add_fact_deducer(dlrn:deduce_dlrn_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dlrn_solver(_) :-
    get_fact(dlrn_problem(Name, Branch)),
    get_fact(dlrn_reproduced(Name, Branch)).

dlrn_solver(Gen) :-
    get_fact(dlrn_problem(Name, Branch)),
    not(download_build_last_dlrn_src(Name, Branch)),
    update_fact(Gen, dlrn_reproduced(Name, Branch)).

:- add_fact_solver(dlrn:dlrn_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dlrn_status(Name, Branch, Status) :-
    get_fact(dlrn_info(Name, Branch, [[Status,_]|_])).

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
    get_fact(dlrn_info(Name, Branch, Info)),
    dlrn_find_last_bad_aux(Info, Sha1).

dlrn_find_last_bad_aux([[failure,Fail],[success, _]|_], Sha1) :-
    basename(Fail, Sha1),
    !.

dlrn_find_last_bad_aux([[failure, _]|Rest], B) :-
    dlrn_find_last_bad_aux(Rest, B).

basename(Name, Base) :-
    split_string(Name, "/", "", List),
    last(List, Base).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dlrn_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member("dlrn", List),
    dlrn_status(Name, Branch, Status),
    member(Name, List),
    format(atom(Answer), "~w: ~w ~w ~w", [Nick, Name, Branch, Status]).

dlrn_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member("dlrn", List),
    findall(Str, dlrn_answer(Str), Res),
    string_join(', ', Res, Concat),
    format(atom(Answer), "~w: ~w", [Nick, Concat]).

dlrn_answer(Answer) :-
    dlrn_status(Name, Branch, Status),
    format(atom(Answer), "~w ~w ~w", [Name, Branch, Status]).

:- add_answerer(dlrn:dlrn_answer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

download_build_last_dlrn_src(Name, Branch) :-
    get_fact(dlrn_info(Name, Branch, [[_,[_,Path,_]]|_])),
    download_build_dlrn_src(Name, Branch, Path).

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

publish_patch(Name, Branch, Pr) :-
    workspace(Ws),
    distgit_workspace(Name, Dir),
    string_concat("rpm-", Branch, DistGitBranch),
    cmd("dlrn_publish_patch.sh ~w ~w/~w/pr-~w.patch ~w", [Dir, Ws, Name, Pr, DistGitBranch]).

remove_patch(Name, Branch, Pr) :-
    workspace(Ws),
    distgit_workspace(Name, Dir),
    string_concat("rpm-", Branch, DistGitBranch),
    cmd("dlrn_unpublish_patch.sh ~w pr-~w.patch ~w", [Dir, Ws, Name, Pr, DistGitBranch]).

%% dlrn.pl ends here
