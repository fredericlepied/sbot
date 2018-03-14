%% -*- prolog -*-

:- module(trello, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(world).
:- use_module(discuss).
:- use_module(config).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_trello_card_facts(Gen) :-
    get_trello_cards(Cards),
    member(Card, Cards),
    get_fact(trello_list(Card.idList, ListName, _, _, BoardName)),
    store_fact(Gen, trello_card(Card.id, Card.name, Card.closed, Card.badges.comments, Card.actions, Card.checklists, Card.shortUrl, Card.idList, ListName, Card.idBoard, BoardName)).

update_trello_list_facts(Gen) :-
    get_trello_board(Board),
    store_fact(Gen, trello_board(Board.id, Board.name, Board.shortUrl)),
    get_trello_lists(Lists),
    member(List, Lists),
    store_fact(Gen, trello_list(List.id, List.name, List.closed, List.idBoard, Board.name)).

:- add_fact_updater(trello:update_trello_card_facts).
:- add_fact_updater(trello:update_trello_list_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% lists: new or unarchived
deduce_trello_facts(Gen) :-
    Gen \== 1,
    get_fact(trello_list(Id, Name, false, BoardId, BoardName)),
    not(get_old_fact(trello_list(Id, _, false,  _, _))),
    store_fact(Gen, new_trello_list(Id, Name, BoardId, BoardName)).

% lists: renamed (open list)
deduce_trello_facts(Gen) :-
    Gen \== 1,
    get_fact(trello_list(Id, Name, false, BoardId, BoardName)),
    get_old_fact(trello_list(Id, OldName, _, _, _)),
    Name \== OldName,
    store_fact(Gen, renamed_trello_list(Id, Name, OldName, BoardId, BoardName)).

% lists: archived list
deduce_trello_facts(Gen) :-
    Gen \== 1,
    get_fact(trello_list(Id, Name, true, BoardId, BoardName)),
    get_old_fact(trello_list(Id, _, false, _, _)),
    store_fact(Gen, archived_trello_list(Id, Name, BoardId, BoardName)).

% cards: new card
deduce_trello_facts(Gen) :-
    Gen \== 1,
    get_fact(trello_card(Id, Name, false, _, _, _, _, _, ListName, _, BoardName)),
    not(get_old_fact(trello_card(Id, _, false, _, _, _, _, _, _, _, _))),
    store_fact(Gen, new_trello_card(Id, Name, ListName, BoardName)).

% cards: moved card
deduce_trello_facts(Gen) :-
    Gen \== 1,
    get_fact(trello_card(Id, Name, false, _, _, _, _, ListId, _, _, BoardName)),
    get_old_fact(trello_card(Id, _, false, _, _, _, _, OldListId, _, _, _)),
    ListId \== OldListId,
    get_fact(trello_list(ListId, ListName, _, _, _)),
    get_fact(trello_list(OldListId, OldListName, _, _, _)),
    store_fact(Gen, moved_trello_card(Id, Name, ListName, OldListName, BoardName)).

% cards: archived card
deduce_trello_facts(Gen) :-
    Gen \== 1,
    get_fact(trello_card(Id, Name, true, _, _, _, _, _, ListName, _, BoardName)),
    get_old_fact(trello_card(Id, _, false, _, _, _, _, _, _, _, _)),
    store_fact(Gen, archived_trello_card(Id, Name, ListName, BoardName)).

% cards: comment card
deduce_trello_facts(Gen) :-
    Gen \== 1,
    get_fact(trello_card(Id, Name, false, CommentNumber, [Comment|_], _, ShortUrl, _, ListName, _, BoardName)),
    get_old_fact(trello_card(Id, _, false, OldCommentNumber, _, _, _, _, _, _, _)),
    OldCommentNumber \== CommentNumber,
    store_fact(Gen, comment_added_trello_card(Id, Name, Comment.memberCreator.fullName, ShortUrl, ListName, BoardName)).

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
    format(string(TrelloUrl), "https://api.trello.com/1/boards/~w/lists?filter=all&key=~w&token=~w", [TrelloBoard, TrelloApiSecret, TrelloOAuthToken]),
    setup_call_cleanup(
        http_open(TrelloUrl, In, []),
        json_read_dict(In, Lists),
        close(In)
    ).

get_trello_cards(Cards) :-
    config(trello_api_secret, TrelloApiSecret),
    config(trello_oauth_token, TrelloOAuthToken),
    config(trello_board, TrelloBoard),
    format(string(TrelloUrl), "https://api.trello.com/1/boards/~w/cards?actions=commentCard&checklists=all&filter=all&key=~w&token=~w", [TrelloBoard, TrelloApiSecret, TrelloOAuthToken]),
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
            bold("trello <board> comment_added_card\n")
           ]).

% trello lists
trello_answer(["trello", "lists"], _, Answer) :-
    findall(Name, get_fact(trello_list(_, Name, false, _, _)), Names),
    sort(Names, NamesSorted),
    string_join(", ", NamesSorted, NamesText),
    format(string(Answer), "Available Lists: ~w", [NamesText]).

:- add_answerer(trello:trello_answer).

%% trello.pl ends here
