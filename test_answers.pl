%% -*- prolog -*-

:- begin_tests(answers).
:- use_module(discuss).

test(answer_hello) :-
    discuss:answer(["hello", "toto"], [irc, "nick"], Answer),
    assertion(Answer == "hello nick").

test(answer_thx) :-
    discuss:answer(["thx"], [irc, "nick"], Answer),
    assertion(Answer == "you're welcome").

test(answer_thx) :-
    discuss:answer(["help"], [irc, "nick"], Answer),
    assertion(Answer == ["use ", bold("<module> help"),
                         ". Available modules: ", bold("self, dlrn, github, puddle, fun, fedora, admin, dci, gerrit, trello"), ".\n",
                         bold("subscribe <topic> [<subtopic or *>...]\n"),
                         bold("unsubscribe <topic> [<subtopic or *>...]\n"),
                         bold("subscriptions")]).

test(answer_default) :-
    discuss:answer([], [irc, "nick"], Answer),
    assertion(Answer == "not understood. Use 'help' to list what I understand.").

test(answer_dlrn_help) :-
    discuss:answer(["dlrn", "help"], [irc, "nick"], Answer),
    assertion(Answer == ["Available commands:\n",
             bold("dlrn"), ": display the status of all the DLRN instances.\n",
             bold("dlrn <package>"), ": display DLRN status for this package.\n",
             bold("dlrn apply pr <pr> to <package> <branch>"), ".\n",
             bold("dlrn remove pr <pr> from <package> <branch>"), ".\n",
             "Available notification:\n",
             bold("dlrn <package> <branch> reproduce")
            ]).

test(answer_github_help) :-
    discuss:answer(["github", "help"], [irc, "nick"], Answer),
    assertion(Answer == ["Available commands:\n",
                         bold("github trackpr"), ": track the speficied PR.\n",
                         bold("github untrackpr"), ": untrack the specified PR.\n"
                        ]).

test(answer_puddle_help) :-
    discuss:answer(["puddle", "help"], [irc, "nick"], Answer),
    assertion(Answer == ["Available commands:\n",
                         bold("puddle"), ": display the list of available puddles.\n",
                         bold("puddle <puddle>"), ": display the list of aliases for this puddle.\n",
                         bold("puddle <puddle> <alias>"), ": display the real puddle name for this alias and its URL.\n",
                         bold("puddle health <puddle> <alias>"), ": display the health check of the puddle.\n",
                         "Available notification:\n",
                         bold("puddle <prodver> new\n"),
                         bold("puddle <prodver> health_check")
                        ]).

test(answer_self_help) :-
    discuss:answer(["self", "help"], [irc, "nick"], Answer),
    assertion(Answer == "version: display the version of my source code.").

test(answer_fun_help) :-
    discuss:answer(["fun", "help"], [irc, "nick"], Answer),
    assertion(Answer == [bold("fortune <word>"),
                         ": fortune message\n",
                         bold("weather <location>"),
                         ": weather at location"]).

:- end_tests(answers).

%% test_answers.pl ends here
