%% -*- prolog -*-

:- module(admin,
          [
          ]).

:- use_module(world).
:- use_module(discuss).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

admin_answer(["admin", "help"], [_, Nick|_],
           ["Available commands:\n",
            bold("kill"), ": kill botsito.\n",
            bold("notunderstood list"), ": list all sentences bot did not unserstand.\n",
            bold("notunderstood remove <sentence>"), ": remove sentence from the not understood sentence databas."
           ]) :-
    config(admins, Admins),
    member(Nick, Admins).

admin_answer(List, _, Answer) :-
    member("admin", List),
    member("help", List),
    format(string(Answer), "~w", ["Not allowed"]).

admin_answer(["kill"], [_,Nick|_], _) :-
    config(admins, Admins),
    member(Nick, Admins),
    halt.

admin_answer(["kill"], _, Answer) :-
    format(string(Answer), "~w", ["Not allowed"]).

admin_answer(["admins"], _, Answer) :-
    config(admins, Admins),
    string_join(", ", Admins, Answer).

admin_answer(["notunderstood", "list"], [_,Nick|_], Answer) :-
    config(admins, Admins),
    member(Nick, Admins),
    findall(Str, get_longterm_fact(not_understood_sentence(Str)), Strs),
    string_join("\n", Strs, Sentences),
    format(string(Answer), "\n~w", [Sentences]).

admin_answer(["notunderstood", "list"], _, Answer) :-
    format(string(Answer), "~w", ["Not allowed"]).

admin_answer(["notunderstood", "remove"|Sentence], [_,Nick|_], Answer) :-
    config(admins, Admins),
    member(Nick, Admins),
    string_join(" ", Sentence, SentenceStr),
    remove_longterm_fact(not_understood_sentence(SentenceStr)),
    format(string(Answer), "~w removed from notunderstood sentence fact", [SentenceStr]).

admin_answer(["notunderstood", "remove"|_], _, Answer) :-
    format(string(Answer), "~w", ["Not allowed"]).

:- add_answerer(admin:admin_answer).

%% admin.pl ends here
