%% -*- prolog -*-

:- module(dlrn,
          [git/2, git_workspace/2, git_extract_pr/2,
           get_dlrn_fact/3, dlrn_last_bad/3, dlrn_status/4,
           build_pr/4, distgit_workspace/2, publish_patch/3
          ]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

:- use_module(config).
:- use_module(discuss).
:- use_module(github).
:- use_module(utils).
:- use_module(req).
:- use_module(world).

:- pkg_or_abort("mock").
:- pkg_or_abort("rpm-build").
:- pkg_or_abort("wget").

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
    dlrn_status(Name, Branch, failure, Path),
    store_fact(Gen, dlrn_problem(Name, Branch, Path)).

% remove PR in package if requested and PR has been merged
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

% update PR in package if requested and PR is updated or not already built
deduce_dlrn_facts(_) :-
    get_longterm_fact(dlrn_apply_pr(Pr, Name, Branch, Context)),
    get_sha(Name, Branch, Pr, Sha),
    get_fact(dlrn_info(Name, Branch, [[_, [_, Path,_]]|_])),
    deduce_dlrn_local_build(Pr, Name, Branch, Context, Path, Sha),
    deduce_dlrn_publish(Pr, Name, Branch, Context, Sha).

% remove PR from package
deduce_dlrn_facts(_) :-
    get_longterm_fact(dlrn_remove_pr(Pr, Name, Branch, Context)),
    (remove_patch(Name, Branch, Pr) -> 
         (format(string(Text), "removed PR ~w from package ~w ~w",
                 [Pr, Name, Branch]),
          notify(Text, Context),
          remove_longterm_fact(dlrn_remove_pr(Pr, Name, Branch, _)),
          remove_longterm_fact(dlrn_apply_pr(Pr, Name, Branch, _)),
          remove_longterm_fact(github_track_pr(_, Name, Pr, _)),
          remove_longterm_fact((dlrn_published(Pr, Name, Branch, _, _))));
     (format(string(Text), "unable to remove PR ~w from package ~w ~w",
             [Pr, Name, Branch]),
      notify(Text, Context))).

% not built sha
get_sha(Name, _, Pr, Sha) :-
    get_fact(github_updated_pr(_, Name, Pr, Sha, _)).

get_sha(Name, Branch, Pr, Sha) :-
    get_fact(github_pr_sha(_, Name, Pr, Sha)),
    not(get_longterm_fact(dlrn_local_build(Pr, Name, Branch, _, Sha))).

% build src.rpm locally with a PR
deduce_dlrn_local_build(Pr, Name, Branch, Context, _, Sha) :-
    get_longterm_fact(dlrn_local_build(Pr, Name, Branch, Context, Sha, _)),
    !.

deduce_dlrn_local_build(Pr, Name, Branch, Context, Path, Sha) :-
    not(get_longterm_fact(dlrn_local_build(Pr, Name, Branch, Context, Sha, _))),
    (build_pr(Name, Branch, Path, Pr) -> Status = success; Status = failure),
    store_longterm_fact(dlrn_local_build(Pr, Name, Branch, Context, Sha, Status)),
    format(string(Text), "built package ~w ~w locally with updated PR ~w (~w): ",
           [Name, Branch, Pr, Sha]),
    colored(Status, Colored),
    notify([Text, Colored], Context).

% dlrn publish patch for PR
deduce_dlrn_publish(Pr, Name, Branch, Context, Sha) :-
    get_longterm_fact(dlrn_published(Pr, Name, Branch, Context, Sha)),
    !.

deduce_dlrn_publish(Pr, Name, Branch, Context, Sha) :-
    not(get_longterm_fact(dlrn_published(Pr, Name, Branch, Context, Sha))),
    publish_patch(Name, Branch, Pr),
    store_longterm_fact(dlrn_published(Pr, Name, Branch, Context, Sha)),
    format(string(Text), "updated dlrn package ~w ~w with PR ~w (~w)", [Name, Branch, Pr, Sha]),
    notify(Text, Context).

% dlrn style path from sha1
dlrn_path(Sha, Path) :-
    sub_string(Sha, 0, 2, _, First),
    sub_string(Sha, 2, 2, _, Second),
    format(string(Path), "~w/~w/~w", [First, Second, Sha]).

colored(success, green("success")).
colored(failure, red("failure")).

:- add_fact_deducer(dlrn:deduce_dlrn_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% already reproduced build issue
dlrn_solver(Gen) :-
    get_fact(dlrn_problem(Name, Branch, Path)),
    get_old_fact(dlrn_reproduced(Name, Branch, Path, Status)),
    store_fact(Gen, dlrn_reproduced(Name, Branch, Path, Status)).

% reproduced build issue if not already tried
dlrn_solver(Gen) :-
    get_fact(dlrn_problem(Name, Branch, RelPath)),
    not(get_old_fact(dlrn_reproduced(Name, Branch, RelPath, _))),
    config(dlrn_status_url, [Name, Branch, Url]),
    download_srcrpm(Url, RelPath, Path),
    !,
    url_workspace("dlrn", WsUrl),
    (build_srcrpm(Path) -> Status = success; Status = failure),
    store_fact(Gen, dlrn_reproduced(Name, Branch, RelPath, Status)),
    (Status == success ->
         format(string(Text), "** DLRN ~w build problem not reproduced for ~w (~w/~w)",
                [Name, Branch, WsUrl, RelPath]);
     format(string(Text), "** DLRN ~w build problem reproduced for ~w (~w/~w)",
            [Name, Branch, WsUrl, RelPath])),
    notification(["dlrn", Name, Branch, "updated_pkg"], Text).

% not able to reproduce a build issue because of a download issue if
% not already tried
dlrn_solver(Gen) :-
    get_fact(dlrn_problem(Name, Branch, RelPath)),
    not(get_old_fact(dlrn_reproduced(Name, Branch, RelPath, _))),
    config(dlrn_status_url, [Name, Branch, _]),
    get_download_error(Name, Branch, RelPath, Issue),
    store_fact(Gen, dlrn_reproduced(Name, Branch, RelPath, download_error)),
    url_workspace("dlrn", WsUrl),
    format(string(Text), "** DLRN ~w build problem not reproduced for ~w (~w: ~w/~w)",
           [Name, Branch, Issue, WsUrl, RelPath]),
    notification(["dlrn", Name, Branch, "reproduce"], Text).
    
:- add_fact_solver(dlrn:dlrn_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dlrn_status(Name, Branch, Status, Path) :-
    get_fact(dlrn_info(Name, Branch, [[Status,[_,Path,_]]|_])).

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
dlrn_answer(List, _, "dlrn: display the status of all the DLRN instances.\ndlrn <package>: display DLRN status for this package.\ndlrn apply pr <pr> to <package> <branch>.\ndlrn remove pr <pr> from <package> <branch>.") :-
    member("dlrn", List),
    member("help", List).

% dlrn apply pr 35917 to ansible devel
dlrn_answer(["dlrn", "apply", "pr", Pr, "to", Name, Branch], Context, Answer) :-
    store_longterm_fact(dlrn_apply_pr(Pr, Name, Branch, Context)),
    github_answer(["github", "trackpr", Name, Name, Pr], Context, _),
    format(string(Answer), "ok added applying PR ~w to ~w ~w to my backlog.", [Pr, Name, Branch]).

% dlrn remove pr 35917 from ansible devel
dlrn_answer(["dlrn", "remove", "pr", Pr, "from", Name, Branch], Context, Answer) :-
    store_longterm_fact(dlrn_remove_pr(Pr, Name, Branch, Context)),
    github_answer(["github", "trackpr", Name, Name, Pr], Context, _),
    format(string(Answer), "ok added removing PR ~w from ~w ~w to my backlog.", [Pr, Name, Branch]).

% dlrn ansible
dlrn_answer(List, _, Answer) :-
    member("dlrn", List),
    dlrn_status(Name, Branch, Status, _),
    member(Name, List),
    format(string(Answer), "~w ~w ~w", [Name, Branch, Status]).

% dlrn
dlrn_answer(["dlrn"], _, Answer) :-
    findall(Str, dlrn_answer(Str), Res),
    string_join(', ', Res, Answer).

dlrn_answer(Answer) :-
    dlrn_status(Name, Branch, Status, _),
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

distgit_workspace(Name, Dir) :-
    string_concat(Name, "-distgit", DistGitName),
    git_workspace(DistGitName, Dir).

git_workspace(Name, Dir) :-
    git(Name, Url),
    workspace("dlrn", W),
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

get_last_path(Name, Branch, Path) :-
    get_fact(dlrn_info(Name, Branch, [[_,[_,Path,_]]|_])).

download_srcrpm(Url, RelPath, Path) :-
    workspace("dlrn", Ws),
    cmd("dlrn_download_srcrpm.sh ~w ~w ~w", [Ws, Url, RelPath]),
    format(string(Path), "~w/~w", [Ws, RelPath]).

build_srcrpm(Path) :-
    cmd("dlrn_build_srcrpm.sh ~w", [Path]).

build_pr(Name, Branch, RelPath, Pr) :-
    config(dlrn_status_url, [Name, Branch, Url]),
    download_srcrpm(Url, RelPath, Path),
    git_extract_pr(Name, Pr),
    workspace("dlrn", Ws),
    cmd("dlrn_inject_patch.sh ~w ~w/~w/pr-~w.patch", [Path, Ws, Name, Pr]),
    build_srcrpm(Path).

publish_patch(Name, Branch, Pr) :-
    workspace("dlrn", Ws),
    distgit_workspace(Name, Dir),
    string_concat("rpm-", Branch, DistGitBranch),
    cmd("dlrn_publish_patch.sh ~w ~w/~w/pr-~w.patch ~w", [Dir, Ws, Name, Pr, DistGitBranch]).

remove_patch(Name, Branch, Pr) :-
    distgit_workspace(Name, Dir),
    string_concat("rpm-", Branch, DistGitBranch),
    cmd("dlrn_unpublish_patch.sh ~w pr-~w.patch ~w", [Dir, Pr, DistGitBranch]).

get_download_error(_, _, RelPath, "invalid or expired") :-
    workspace("dlrn", Ws),
    format(string(Filename), "~w/~w/copr.log", [Ws, RelPath]),
    read_file_to_string(Filename, String, []),
    split_string(String, " \n", "", List),
    member("invalid/expired.", List),
    !.

get_download_error(_, _, _, "download error").

%% dlrn.pl ends here
