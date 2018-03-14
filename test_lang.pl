%% -*- prolog -*-

:- use_module(lang).
:- use_module(discuss).

test_phrase(String) :-
    test_phrase(String, R),
    writeln(R).

test_phrase(String, Result) :-
    split_words(String, Words),
    lang:strings_atoms(Words, Atoms),
    writeln(Atoms),
    phrase(sentence(Result), Atoms).

:- begin_tests(test_lang).

test(basic) :- test_phrase("build the ansible package").
test(basic2) :- test_phrase("build ansible").

test(question) :- test_phrase("what is the status of osp12 job ?").
test(question2) :- test_phrase("what is the status of the osp12 job for dell?").

test(which) :- test_phrase("which partners are testing osp12?").

test(apply) :- test_phrase("apply pr 42 on ansible").

test(track) :- test_phrase("could you track pr 42 on the ansible package ?").

:- end_tests(test_lang).

%% test_lang.pl ends here
