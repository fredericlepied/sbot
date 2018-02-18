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
    assertion(Answer == "use <module> help. Available modules: dlrn, self, github, puddle, fun").

test(answer_default) :-
    discuss:answer([], [irc, "nick"], Answer),
    assertion(Answer == "not understood. Use 'help' to list what I understand.").

test(answer_dlrn_help) :-
    discuss:answer(["dlrn", "help"], [irc, "nick"], Answer),
    assertion(Answer == "dlrn: display the status of all the DLRN instances.\ndlrn <package>: display DLRN status for this package.\ndlrn apply pr <pr> to <package> <branch>.").

test(answer_github_help) :-
    discuss:answer(["github", "help"], [irc, "nick"], Answer),
    assertion(Answer == "github trackpr <owner> <project> <pr>\ngithub untrackpr <owner> <project> <pr>\ngithub trackpr: list all the tracked PR").

test(answer_puddle_help) :-
    discuss:answer(["puddle", "help"], [irc, "nick"], Answer),
    assertion(Answer == "puddle: display the list of available puddles.\npuddle <puddle>: display the list of aliases for this puddle.\npuddle <puddle> <alias>: display the real puddle name for this alias and its URL.").

test(answer_self_help) :-
    discuss:answer(["self", "help"], [irc, "nick"], Answer),
    assertion(Answer == "version: display the version of my source code.").

test(answer_fun_help) :-
    discuss:answer(["fun", "help"], [irc, "nick"], Answer),
    assertion(Answer == "fortune <word>: furtune message").

:- end_tests(answers).

%% test_answers.pl ends here
