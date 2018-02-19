%% -*- prolog -*-

:- module(dlrn,
          [git/2, workspace/1, git_workspace/2, git_extract_pr/2,
           get_dlrn_fact/3, dlrn_last_bad/3, dlrn_status/3,
           download_build_last_dlrn_src/2, download_build_dlrn_src/3,
           build_pr/4, distgit_workspace/2, publish_patch/3
          ]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

:- use_module(config).
:- use_module(discuss).
:- use_module(github).
:- use_module(utils).
:- use_module(world).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_dlrn_facts(Gen) :-
    config(dlrn_status_url, [Name, Branch, _]),
    get_dlrn_fact(Name, Branch, Info),
    store_fact(Gen, dlrn_info(Name, Branch, Info)).

:- add_fact_updater(dlrn:update_dlrn_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% detect failure
deduce_dlrn_facts(Gen) :-
    dlrn_status(Name, Branch, failure),
    store_fact(Gen, dlrn_problem(Name, Branch)).

% remove PR in package if requested and not already done
deduce_dlrn_facts(_) :-
    get_fact(github_pr_merged(Name, Name, Pr, yes)),
    get_longterm_fact(dlrn_apply_pr(Pr, Name, Branch, Context)),
    get_longterm_fact(dlrn_published(Pr, Name, Branch, Context, Path)),
    remove_patch(Name, Branch, Pr),
    format(string(Text), "removed PR ~w from package ~w ~w (as it was merged)",
           [Pr, Name, Branch]),
    notify(Text, Context),
    remove_longterm_fact(dlrn_apply_pr(Pr, Name, Branch, Context)),
    remove_longterm_fact(dlrn_published(Pr, Name, Branch, Context, Path)).

% update PR in package if requested and not already done
deduce_dlrn_facts(_) :-
    get_longterm_fact(dlrn_apply_pr(Pr, Name, Branch, Context)),
    get_fact(dlrn_info(Name, Branch, [[_, [_, Path,_]]|_])),
    deduce_dlrn_local_build(Pr, Name, Branch, Context, Path),
    deduce_dlrn_build(Pr, Name, Branch, Context, Path).

deduce_dlrn_local_build(Pr, Name, Branch, Context, Path) :-
    get_longterm_fact(dlrn_local_build(Pr, Name, Branch, Context, Path)),
    !.

deduce_dlrn_local_build(Pr, Name, Branch, Context, Path) :-
    not(get_longterm_fact(dlrn_local_build(Pr, Name, Branch, Context, Path))),
    build_pr(Name, Branch, Path, Pr),
    store_longterm_fact(dlrn_local_build(Pr, Name, Branch, Context, Path)),
    format(string(Text), "built package ~w ~w locally with updated PR ~w (~w)",
           [Name, Branch, Pr, Path]),
    notify(Text, Context).

deduce_dlrn_build(Pr, Name, Branch, Context, Path) :-
    get_longterm_fact(dlrn_published(Pr, Name, Branch, Context, Path)),
    !.

deduce_dlrn_build(Pr, Name, Branch, Context, Path) :-
    not(get_longterm_fact(dlrn_published(Pr, Name, Branch, Context, Path))),
    publish_patch(Name, Branch, Pr),
    store_longterm_fact(dlrn_published(Pr, Name, Branch, Context, Path)),
    format(string(Text), "updated dlrn package ~w ~w with PR ~w", [Name, Branch, Pr]),
    notify(Text, Context).

:- add_fact_deducer(dlrn:deduce_dlrn_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% already reproduced build issue
dlrn_solver(_) :-
    get_fact(dlrn_problem(Name, Branch)),
    get_fact(dlrn_reproduced(Name, Branch)).

% reproduced build issue
dlrn_solver(Gen) :-
    get_fact(dlrn_problem(Name, Branch)),
    not(download_build_last_dlrn_src(Name, Branch)),
    store_fact(Gen, dlrn_reproduced(Name, Branch)),
    format(string(Text), "** DLRN ansible build problem reproduced for ~w ~w", [Name, Branch]),
    notify(Text, []).

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
    config(dlrn_status_url, [Name, Branch, BaseUrl]),
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

% dlrn help
dlrn_answer(List, _, "dlrn: display the status of all the DLRN instances.\ndlrn <package>: display DLRN status for this package.\ndlrn apply pr <pr> to <package> <branch>.") :-
    member("dlrn", List),
    member("help", List).

% dlrn apply pr 35917 to ansible devel
dlrn_answer(["dlrn", "apply", "pr", Pr, "to", Name, Branch], Context, Answer) :-
    store_longterm_fact(dlrn_apply_pr(Pr, Name, Branch, Context)),
    github_answer(["github", "trackpr", Name, Name, Pr], Context, _),
    format(string(Answer), "ok added PR ~w to ~w ~w to my backlog.", [Pr, Name, Branch]).

% dlrn ansible
dlrn_answer(List, _, Answer) :-
    member("dlrn", List),
    dlrn_status(Name, Branch, Status),
    member(Name, List),
    format(string(Answer), "~w ~w ~w", [Name, Branch, Status]).

% dlrn
dlrn_answer(List, _, Answer) :-
    member("dlrn", List),
    findall(Str, dlrn_answer(Str), Res),
    string_join(', ', Res, Answer).

dlrn_answer(Answer) :-
    dlrn_status(Name, Branch, Status),
    format(string(Answer), "~w ~w ~w", [Name, Branch, Status]).

:- add_answerer(dlrn:dlrn_answer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

git(Name, GitUrl) :-
    config(gitrepo, [Name, GitUrl]),
    !.

git(Project, GitUrl) :-
    config(github, [Owner, Project]),
    format(string(GitUrl), "https://github.com/~w/~w", [Owner, Project]).

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
    config(dlrn_status_url, [Name, Branch, Url]),
    workspace(Ws),
    cmd("dlrn_download_srcrpm.sh ~w ~w ~w", [Ws, Url, Path]),
    cmd("dlrn_build_srcrpm.sh ~w ~w", [Ws, Path]).

build_pr(Name, Branch, Path, Pr) :-
    config(dlrn_status_url, [Name, Branch, Url]),
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
