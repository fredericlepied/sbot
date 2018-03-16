%% -*- prolog -*-

:- module(gerrit, [gerrit_query/2]).

:- use_module(library(http/json)).

:- use_module(gerritlib).
:- use_module(config).
:- use_module(discuss).
:- use_module(utils).
:- use_module(world).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_gerrit_facts(Gen) :-
    config(gerrit_projects, Projects),
    string_join("|", Projects, Regexp),
    format(string(Query), "status:open 'project:^~w'", Regexp),
    gerrit_query(Query, Dicts),
    findall(D, (member(D, Dicts),
                store_fact(Gen, gerrit_open_review(D.project, D.number, D.owner.name,
                                                   D.subject, D.lastUpdated))),
            _).

update_gerrit_facts(Gen) :-
    not(get_midterm_fact(init_gen(gerrit, _))),
    store_midterm_fact(init_gen(gerrit, Gen)).

:- add_fact_updater(gerrit:update_gerrit_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use_stream :-
    config(gerrit_stream, yes).

% update on a review deduced from polling
deduce_gerrit_facts(Gen) :-
    not(use_stream),
    get_fact(gerrit_open_review(Project, Number, Owner, Subject, LastUpdated)),
    get_old_fact(gerrit_open_review(Project, Number, Owner, _, OldLastUpdated)),
    LastUpdated \== OldLastUpdated,
    store_fact(Gen, gerrit_review_updated(Project, Number, Owner, Subject,
                                          LastUpdated, OldLastUpdated)),
    gerrit_url(Number, Url),
    format(string(Text), "** review ~w from ~w updated on ~w: ~w (~w)",
           [Number, Owner, Project, Subject, Url]),
    notification(["gerrit", Project, "updated"], Text).

% new review deduced from polling
deduce_gerrit_facts(Gen) :-
    not(use_stream),
    get_midterm_fact(init_gen(gerrit, InitGen)),
    Gen \== InitGen,
    get_fact(gerrit_open_review(Project, Number, Owner, Subject, LastUpdated)),
    not(get_old_fact(gerrit_open_review(Project, Number, Owner, _, _))),
    store_fact(Gen, gerrit_review_updated(Project, Number, Owner, Subject, LastUpdated)),
    gerrit_url(Number, Url),
    format(string(Text), "** new review ~w from ~w on ~w: ~w (~w)",
           [Number, Owner, Project, Subject, Url]),
    notification(["gerrit", Project, "new"], Text).

% merged of a review deduced from polling
deduce_gerrit_facts(Gen) :-
    not(use_stream),
    get_old_fact(gerrit_open_review(Project, Number, Owner, Subject, _)),
    not(get_fact(gerrit_open_review(Project, Number, Owner, _, _))),
    store_fact(Gen, gerrit_review_merged(Project, Number, Owner, Subject)),
    gerrit_url(Number, Url),
    format(string(Text), "** review ~w from ~w merged on ~w: ~w (~w)",
           [Number, Owner, Project, Subject, Url]),
    notification(["gerrit", Project, "merged"], Text).

% deduced from the event stream

deduce_gerrit_facts(Gen) :-
    deduce_gerrit_facts_from_event(Gen, 'patchset-created', "patchset updated", "patchset-updated", no).

deduce_gerrit_facts(Gen) :-
    deduce_gerrit_facts_from_event(Gen, 'comment-added', "comment added", "comment-added", yes).

deduce_gerrit_facts(Gen) :-
    deduce_gerrit_facts_from_event(Gen, 'change-merged', "change merged", "changed-merged", no).

deduce_gerrit_facts(Gen) :-
    deduce_gerrit_facts_from_event(Gen, 'change-abandoned', "change abandoned", "changed-abandoned", no).

deduce_gerrit_facts_from_event(Gen, Type, Explain, NotifType, FilterUsers) :-
    get_fact(gerrit_event(Type, Project, Number, D)),
    config(gerrit_projects, Projects),
    member(Project, Projects),
    get_config(gerrit_ignore_users, Users, []),
    (FilterUsers == yes ->
         (not(member(D.author.username, Users)),
          store_fact(Gen, gerrit_review_changed(Type, Project, Number, D.author.name,
                                                D.change.subject, D.'eventCreatedOn')),
          Name = D.author.name);
     Name = D.change.owner.name),
    gerrit_url(Number, Url),
    format(string(Text), "** review ~w from ~w ~w to ~w: ~w (~w)",
           [Number, Name, Explain, Project, D.change.subject, Url]),
    notification(["gerrit", Project, NotifType], Text).

:- add_fact_deducer(gerrit:deduce_gerrit_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gerrit_version(Version) :-
    gerrit_cmd("version", [], Stream),
    read_string(Stream, _, Version),
    close(Stream).

gerrit_query(QueryArgs, Results) :-
    gerrit_cmd("query --format=JSON ~w", [QueryArgs], Stream),
    get_results(Stream, AllResults),
    % remove the last element as it is statistics
    reverse(AllResults, [_|Reversed]),
    reverse(Reversed, Results).

get_results(Stream, []) :-
    at_end_of_stream(Stream),
    close(Stream),
    !.

get_results(Stream, [Result|Rest]) :-
    json_read_dict(Stream, Result),
    get_results(Stream, Rest).

gerrit_cmd(SubCmd, Args, Stream) :-
    config(gerrit_access, [User, Server, Port]),
    format(string(Cmd), SubCmd, Args),
    format(string(FullCmd), "ssh -p ~w ~w@~w gerrit ~w", [Port, User, Server, Cmd]),
    open(pipe(FullCmd), read, Stream).

gerrit_url(Number, Url) :-
    config(gerrit_access, [_, Server, _]),
    format(string(Url), "https://~w/r/~w", [Server, Number]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gerrit_answer(["gerrit", "help"], _, ["Available commands:\n",
                                      bold("gerrit version"), ": return gerrit version\n",
                                      bold("gerrit projects"), ": list projects with open reviews\n",
                                      bold("gerrit reviews <project>"), ": display open reviews for <project>\n",
                                      "Available notifications:\n",
                                      bold("gerrit <project> new"), "\n",
                                      bold("gerrit <project> updated"), "\n",
                                      bold("gerrit <project> patchset-updated"), "\n",
                                      bold("gerrit <project> comment-added"), "\n",
                                      bold("gerrit <project> change-merged"), "\n",
                                      bold("gerrit <project> change-abandoned"), "\n",
                                      bold("gerrit <project> merged")
                                     ]).

gerrit_answer(["gerrit", "version"], _, Answer) :-
    gerrit_version(Answer).

gerrit_answer(["gerrit", "projects"], _, Answer) :-
    findall(Project, get_fact(gerrit_open_review(Project, _, _, _, _)),
            Projects),
    sort(Projects, Texts),
    string_join(", ", Texts, Joined),
    length(Texts, Len),
    format(string(Answer), "~w projects with open reviews: ~w", [Len, Joined]).

gerrit_answer(["gerrit", "reviews", Project], _, Answer) :-
    findall(Text, (get_fact(gerrit_open_review(Project, Number, Owner, Subject, _)),
                   gerrit_url(Number, Url),
                   format(string(Text), "- ~w from ~w: ~w (~w)", [Number, Owner, Subject, Url])),
            Texts),
    compute_reviews_answer(Project, Texts, Answer).

compute_reviews_answer(Project, [], Answer) :-
    format(string(Answer), "No open reviews for ~w", [Project]).

compute_reviews_answer(Project, Texts, Answer) :-
    string_join("\n", Texts, Joined),
    length(Texts, Len),
    format(string(Answer), "~w open reviews for ~w:\n~w", [Len, Project, Joined]).

:- add_answerer(gerrit:gerrit_answer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% thread to read events from gerrit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% not configured to use event stream
gerrit_run :-
    not(use_stream),
    !.

% already running
gerrit_run :-
    is_thread(gerrit_cmd),
    !.

% real launch if settings exist
gerrit_run :-
    config(gerrit_access, [User, Server, Port]),
    !,
    thread_create(gerrit_loop(User, Server, Port), _, [detached(true), alias(gerrit_cmd)]).

% no settings
gerrit_run :-
    not(config(gerrit_access, [_, _, _])).

gerrit_loop(User, Server, Port) :-
    repeat,
    catch(
        thread_create(process_gerrit_stream(User, Server, Port), _, [alias(gerrit)]),
	Err,
	print_message(error, Err)
    ),
    thread_join(gerrit, _),
    writeln("Gerrit stream lost, attempting to reconnect ..."),
    sleep(30),
    fail.

process_gerrit_stream(User, Server, Port) :-
    format(string(Cmd), "ssh -p ~w ~w@~w gerrit stream-events < /dev/null", [Port, User, Server]),
    %format(string(Cmd), "cat /var/tmp/stream.json", [Port, User, Server]),
    open(pipe(Cmd), read, Stream),
    get_gerrit_event(Stream).

get_gerrit_event(Stream) :-
    at_end_of_stream(Stream),
    close(Stream),
    !.

get_gerrit_event(Stream) :-
    json_read_dict(Stream, Dict),
    store_gerrit_fact(Dict),
    !,
    get_gerrit_event(Stream).

% do not store these events
store_gerrit_fact(D) :-
    member(D.type, ["ref-replicated", "ref-replication-done", "ref-updated"]).

% store an event with the change attribute according to
% https://gerrit-review.googlesource.com/Documentation/cmd-stream-events.html
store_gerrit_fact(D) :-
    member(D.type, ["assignee-changed", "change-abandoned", "change-merged", "change-restored", "comment-added", "hashtags-changed", "patchset-created", "reviewer-added", "reviewer-deleted", "topic-changed", "wip-state-changed", "private-state-changed", "vote-deleted"]),
    atom_string(Atom, D.type),
    store_nextgen_fact(gerrit_event(Atom, D.change.project, D.change.number, D)).

% event without a change attribute
store_gerrit_fact(D) :-
    atom_string(Atom, D.type),
    store_nextgen_fact(gerrit_event(Atom, D)).

:- gerrit_run.

%% gerrit.pl ends here
