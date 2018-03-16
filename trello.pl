%% -*- prolog -*-

:- module(trello, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(world).
:- use_module(discuss).
:- use_module(config).
:- use_module(utils).
:- use_module(githublib).
:- use_module(gerritlib).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_trello_facts(Gen) :-
    get_trello_board(Board),
    store_fact(Gen, trello_board(Board.id, Board.name, Board.shortUrl)),
    get_trello_lists(Lists),
    member(List, Lists),
    store_fact(Gen, trello_list(List.id, List.name, List.closed, List.idBoard, Board.name)).

update_trello_facts(Gen) :-
    get_trello_cards(Cards),
    member(Card, Cards),
    get_fact(trello_list(Card.idList, ListName, _, _, BoardName)),
    store_fact(Gen, trello_card(Card.id, Card.name, Card.closed, Card.badges.comments, Card.actions, Card.checklists, Card.shortUrl, Card.idList, ListName, Card.idBoard, BoardName)).

update_trello_facts(Gen) :-
    not(get_midterm_fact(init_gen(trello, _))),
    store_midterm_fact(init_gen(trello, Gen)).

:- add_fact_updater(trello:update_trello_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_first_iteration(Gen) :-
    get_midterm_fact(init_gen(trello, InitGen)),
    Gen \== InitGen.

% lists: new or unarchived
deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_fact(trello_list(Id, Name, _, BoardId, BoardName)),
    not(get_old_fact(trello_list(Id, _, _,  _, _))),
    store_fact(Gen, new_trello_list(Id, Name, BoardId, BoardName)).

% lists: renamed (open list)
deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_fact(trello_list(Id, Name, false, BoardId, BoardName)),
    get_old_fact(trello_list(Id, OldName, _, _, _)),
    Name \== OldName,
    store_fact(Gen, renamed_trello_list(Id, Name, OldName, BoardId, BoardName)).

% lists: archived list
deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_fact(trello_list(Id, Name, true, BoardId, BoardName)),
    get_old_fact(trello_list(Id, _, false, _, _)),
    store_fact(Gen, archived_trello_list(Id, Name, BoardId, BoardName)).

deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_old_fact(trello_list(Id, Name, _, BoardId, BoardName)),
    not(get_fact(trello_list(Id, _, _, _, _))),
    store_fact(Gen, archived_trello_list(Id, Name, BoardId, BoardName)).

% cards: new card
deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_fact(trello_card(Id, Name, false, _, _, _, _, _, ListName, _, BoardName)),
    not(get_old_fact(trello_card(Id, _, false, _, _, _, _, _, _, _, _))),
    store_fact(Gen, new_trello_card(Id, Name, ListName, BoardName)).

% cards: moved card
deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_fact(trello_card(Id, Name, false, _, _, _, _, ListId, _, _, BoardName)),
    get_old_fact(trello_card(Id, _, false, _, _, _, _, OldListId, _, _, _)),
    ListId \== OldListId,
    get_fact(trello_list(ListId, ListName, _, _, _)),
    get_fact(trello_list(OldListId, OldListName, _, _, _)),
    store_fact(Gen, moved_trello_card(Id, Name, ListName, OldListName, BoardName)).

% cards: archived card
deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_fact(trello_card(Id, Name, true, _, _, _, _, _, ListName, _, BoardName)),
    get_old_fact(trello_card(Id, _, false, _, _, _, _, _, _, _, _)),
    store_fact(Gen, archived_trello_card(Id, Name, ListName, BoardName)).

deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_old_fact(trello_card(Id, Name, _, _, _, _, _, _, ListName, _, BoardName)),
    not(get_fact(trello_card(Id, _, _, _, _, _, _, _, _, _, _))),
    store_fact(Gen, archived_trello_card(Id, Name, ListName, BoardName)).

% cards: comment card
deduce_trello_facts(Gen) :-
    not_first_iteration(Gen),
    get_fact(trello_card(Id, Name, false, CommentNumber, [Comment|_], _, ShortUrl, _, ListName, _, BoardName)),
    get_old_fact(trello_card(Id, _, false, OldCommentNumber, _, _, _, _, _, _, _)),
    OldCommentNumber \== CommentNumber,
    store_fact(Gen, comment_added_trello_card(Id, Name, Comment.memberCreator.fullName, ShortUrl, ListName, BoardName)).

% cards: checklist items matches github issue/pr or gerrit review
deduce_trello_facts(_) :-
    get_fact(trello_card(CardId, _, false, _, _, CardChecklists, _, _, _, _, _)),
    member(List, CardChecklists),
    member(CheckItem, List.checkItems),
    CheckItem.state == "incomplete",
    split_string(CheckItem.name, " ", "()\"", ListOfWords),
    member(Url, ListOfWords),
    sub_string(Url, 0, _, _, "http"),
    check_url_type(Url, CheckItem, CardId).

check_url_type(Url, CheckItem, CardId) :-
    is_github_issue(Url, Owner, Project, IssueId),
    get_github_issue(Owner, Project, IssueId, _),
    store_midterm_fact(trello_track_github_issue(CardId, CheckItem.id, CheckItem.name, Owner, Project, IssueId)).

check_url_type(Url, CheckItem, CardId) :-
    is_github_pr(Url, Owner, Project, PullRequestId),
    get_github_pr(Owner, Project, PullRequestId, _),
    store_midterm_fact(trello_track_github_pr(CardId, CheckItem.id, CheckItem.name, Owner, Project, PullRequestId)).

check_url_type(Url, CheckItem, CardId) :-
    is_gerrit_review(Url, ReviewId, BaseUrl),
    get_fact(gerrit_open_review(_, ReviewId, _, _, _)),
    store_midterm_fact(trello_track_gerrit_review(CardId, CheckItem.id, CheckItem.name, ReviewId, BaseUrl)).

check_url_type(Url, CheckItem, CardId) :-
    is_gerrit_review(Url, ReviewId, BaseUrl),
    get_gerrit_review(BaseUrl, ReviewId, _),
    store_midterm_fact(trello_track_gerrit_review(CardId, CheckItem.id, CheckItem.name, ReviewId, BaseUrl)).

:- add_fact_deducer(trello:deduce_trello_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trello_solver(_) :-
    get_fact(new_trello_list(_, Name, _, BoardName)),
    format(string(Text), "** [~w] New list \"~w\"", [BoardName, Name]),
    notification(["trello", BoardName, "new_list"], Text).

trello_solver(_) :-
    get_fact(renamed_trello_list(_, Name, OldName, _, BoardName)),
    format(string(Text), "** [~w] List \"~w\" has been renamed to \"~w\"", [BoardName, OldName, Name]),
    notification(["trello", BoardName, "renamed_list"], Text).

trello_solver(_) :-
    get_fact(archived_trello_list(_, Name, _, BoardName)),
    format(string(Text), "** [~w] List \"~w\" has been archived", [BoardName, Name]),
    notification(["trello", BoardName, "archived_list"], Text).

trello_solver(_) :-
    get_fact(new_trello_card(_, Name, ListName, BoardName)),
    format(string(Text), "** [~w] New card \"~w\" added to \"~w\"", [BoardName, Name, ListName]),
    notification(["trello", BoardName, "new_card"], Text).

trello_solver(_) :-
    get_fact(moved_trello_card(_, Name, ListName, OldListName, BoardName)),
    format(string(Text), "** [~w] Card \"~w\" moved from \"~w\" to \"~w\"", [BoardName, Name, OldListName, ListName]),
    notification(["trello", BoardName, "moved_card"], Text).

trello_solver(_) :-
    get_fact(archived_trello_card(_, Name, _, BoardName)),
    format(string(Text), "** [~w] Card \"~w\" has been archived", [BoardName, Name]),
    notification(["trello", BoardName, "archived_card"], Text).

trello_solver(_) :-
    get_fact(comment_added_trello_card(_, Name, Author, ShortUrl, _, BoardName)),
    format(string(Text), "** [~w] ~w made a comment on \"~w\" (~w)", [BoardName, Author, Name, ShortUrl]),
    notification(["trello", BoardName, "comment_added_card"], Text).

trello_solver(_) :-
    get_midterm_fact(trello_track_github_issue(CardId, CheckItemId, Url, Owner, Project, Id)),
    get_github_issue(Owner, Project, Id, Issue),
    Issue.state == "closed",
    update_trello_checklist(CardId, CheckItemId),
    get_fact(trello_card(CardId, CardName, _, _, _, _, CardUrl, _, _, _, BoardName)),
    format(string(Text), "** [~w] ~w has been checked on \"~w\" (~w) [github issue]", [BoardName, Url, CardName, CardUrl]),
    notification(["trello", BoardName, "checklist_marked_done_card"], Text),
    remove_midterm_fact(trello_track_github_issue(CardId, CheckItemId, Url, Owner, Project, Id)).

trello_solver(_) :-
    get_midterm_fact(trello_track_github_pr(CardId, CheckItemId, Url, Owner, Project, Id)),
    get_github_pr(Owner, Project, Id, PullRequest),
    PullRequest.state == "closed",
    update_trello_checklist(CardId, CheckItemId),
    get_fact(trello_card(CardId, CardName, _, _, _, _, CardUrl, _, _, _, BoardName)),
    format(string(Text), "** [~w] ~w has been checked on \"~w\" (~w) [github pr]", [BoardName, Url, CardName, CardUrl]),
    notification(["trello", BoardName, "checklist_marked_done_card"], Text),
    remove_midterm_fact(trello_track_github_pr(CardId, CheckItemId, Url, Owner, Project, Id)).

trello_solver(_) :-
    get_midterm_fact(trello_track_gerrit_review(CardId, CheckItemId, Url, ReviewId, BaseUrl)),
    get_gerrit_review(BaseUrl, ReviewId, Review),
    Review.status == "MERGED",
    update_trello_checklist(CardId, CheckItemId),
    get_fact(trello_card(CardId, CardName, _, _, _, _, CardUrl, _, _, _, BoardName)),
    format(string(Text), "** [~w] ~w has been checked on \"~w\" (~w) [gerrit review]", [BoardName, Url, CardName, CardUrl]),
    notification(["trello", BoardName, "checklist_marked_done_card"], Text),
    remove_midterm_fact(trello_track_gerrit_review(CardId, CheckItemId, Url, ReviewId, BaseUrl)).

:- add_fact_solver(trello:trello_solver).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_trello_board(Board) :-
    config(trello_api_secret, TrelloApiSecret),
    config(trello_oauth_token, TrelloOAuthToken),
    config(trello_board, TrelloBoard),
    format(string(TrelloUrl), "https://api.trello.com/1/boards/~w?&key=~w&token=~w", [TrelloBoard, TrelloApiSecret, TrelloOAuthToken]),
    setup_call_cleanup(
        http_open(TrelloUrl, In, []),
        json_read_dict(In, Board),
        close(In)
    ).

get_trello_lists(Lists) :-
    config(trello_api_secret, TrelloApiSecret),
    config(trello_oauth_token, TrelloOAuthToken),
    config(trello_board, TrelloBoard),
    format(string(TrelloUrl), "https://api.trello.com/1/boards/~w/lists/open?filter=all&key=~w&token=~w", [TrelloBoard, TrelloApiSecret, TrelloOAuthToken]),
    setup_call_cleanup(
        http_open(TrelloUrl, In, []),
        json_read_dict(In, Lists),
        close(In)
    ).

get_trello_cards(Cards) :-
    config(trello_api_secret, TrelloApiSecret),
    config(trello_oauth_token, TrelloOAuthToken),
    config(trello_board, TrelloBoard),
    format(string(TrelloUrl), "https://api.trello.com/1/boards/~w/cards/visible?actions=commentCard&checklists=all&filter=all&key=~w&token=~w", [TrelloBoard, TrelloApiSecret, TrelloOAuthToken]),
    setup_call_cleanup(
        http_open(TrelloUrl, In, []),
        json_read_dict(In, Cards),
        close(In)
    ).

update_trello_checklist(CardId, CheckItemId) :-
    config(trello_api_secret, TrelloApiSecret),
    config(trello_oauth_token, TrelloOAuthToken),
    format(string(TrelloUrl), "https://api.trello.com/1/cards/~w/checkItem/~w?key=~w&token=~w", [CardId, CheckItemId, TrelloApiSecret, TrelloOAuthToken]),
    http_put(TrelloUrl, form([state = "complete"]), _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% trello help
trello_answer(["trello", "help"], _,
           ["Available commands:\n",
            bold("trello lists"), ": display the list of available products.\n",
            "Available notifications\n",
            bold("trello <board> new_list\n"),
            bold("trello <board> renamed_list\n"),
            bold("trello <board> archived_list\n"),
            bold("trello <board> new_card\n"),
            bold("trello <board> moved_card\n"),
            bold("trello <board> archived_card\n"),
            bold("trello <board> comment_added_card\n"),
            bold("trello <board> checklist_marked_done_card\n")
           ]).

% trello lists
trello_answer(["trello", "lists"], _, Answer) :-
    findall(Name, get_fact(trello_list(_, Name, false, _, _)), Names),
    sort(Names, NamesSorted),
    string_join(", ", NamesSorted, NamesText),
    format(string(Answer), "Available Lists: ~w", [NamesText]).

:- add_answerer(trello:trello_answer).

%% trello.pl ends here
