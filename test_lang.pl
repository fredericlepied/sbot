%% -*- prolog -*-

:- begin_tests(test_lang).

:- use_module(lang).

test(basic) :-
    phrase(sentence(build(package(ansible))), [build,the,ansible,package]).

test(question) :-
    phrase(sentence(find(job(osp12))), [what,is,the,status,of,osp12,job,'?']).

test(question2) :-
    phrase(sentence(find(job(osp12), partner(dell))), [what,is,the,status,of,the,osp12,job,for,dell,'?']).

:- end_tests(test_lang).


%% test_lang.pl ends here
