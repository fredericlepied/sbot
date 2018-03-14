%% -*- prolog -*-

:- module(lang, [sentence/3]).

:- discontiguous noun/3.

:- use_module(discuss).
:- use_module(utils).

sentence(V) --> polite, verbal_group(V), ['?'].
sentence(V) --> verbal_group(V).
sentence(find(O,A,C)) --> pronoun, object(O), be, action(A), object(C), ['?'].
sentence(find(O)) --> pronoun, be, object(O), ['?'].
sentence(find(O, C)) --> pronoun, be, object(O), complement(C), ['?'].
sentence(count(O)) --> how_many, object(O), interrogation_verb, ['?'].
sentence(count(O, C)) --> how_many, object(O), interrogation_verb, complement(C), ['?'], {validate_count(O,C)}.

how_many --> [how,many].
how_many --> polite, [count,how,many].

interrogation_verb --> [do,we,have].
interrogation_verb --> [have,we].
interrogation_verb --> [do,we,run].
interrogation_verb --> [do,we,possess].

polite --> [can,you].
polite --> [could,you].
polite --> [].

pronoun --> [what].
pronoun --> [which].

be --> [is].
be --> [are].

det --> [the].
det --> [].

action(testing) --> [testing].

verbal_group(apply(O)) --> [apply], object(O).
verbal_group(build(O)) --> [build], object(O).
verbal_group(follow(O)) --> [track], object(O).
verbal_group(follow(O)) --> [follow], object(O).

object(obj(N)) --> det, noun(N).
object(obj(N, C)) --> det, noun(N), complement(C).

complement(N) --> [on], object(N).
complement(N) --> [for], object(N).
complement(N) --> [at], object(N).

noun(pr(Pr)) --> github_pr, number(Pr).
noun(pr(_)) --> github_pr.
noun(issue(Issue)) --> github_issue, number(Issue).
noun(issue(_)) --> github_issue.
noun(review(Review)) --> gerrit_review, number(Review).
noun(review(_)) --> gerrit_review.
noun(project(Project)) --> project(Project), [project].
noun(project(Project)) --> [project], project(Project).
noun(project(Project)) --> project(Project).
noun(project(_)) --> [project].
noun(package(Package)) --> [package], package(Package).
noun(package(Package)) --> package(Package), [package].
noun(package(Package)) --> package(Package).
noun(package(_)) --> [package].
noun(job(Job)) --> job(Job).
noun(job(_)) --> [job].
noun(topic(Topic)) --> topic(Topic).
noun(partner(Partner)) --> partner(Partner).
noun(partner(_)) --> [partner].
noun(partner(_)) --> [partners].
moun(card(Card)) --> [trello,card], card(Card).
moun(card(Card)) --> [card], card(Card).
moun(card(_)) --> [card].
noun(status(O)) --> [status,of], object(O).

github_pr --> [pr].
github_pr --> [github,pr].

github_issue --> [issue].
github_issue --> [github,issue].

gerrit_review --> [review].
gerrit_review --> [gerrit,review].

%number(I, [N|R], R) :-
%    atom_number(N, I).

number(42, ['42'|R], R).

project(ansible, [ansible|R], R).
project(systemd, [systemd|R], R).

package(ansible, [ansible|R], R).

job(Topic) --> topic(Topic), [job].

topic(osp12, [osp12|R], R).
topic(osp13, [osp13|R], R).

card(card1, [card1|R], R).

partner(dell, [dell|R], R).

validate_count(A, B) :-
    validate_apply(A, B).
validate_count(partner(_), topic(_)).

validate_apply(pr(_), package(_)).
validate_apply(review(_), package(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strings_atoms(ListOfStrings, ListOfAtoms) :-
    map(string_lower, ListOfStrings, Lowers),
    map(string_atom, Lowers, ListOfAtoms).
    
lang_answer(["lang"|List], _, Answer) :-
    strings_atoms(List, Atoms),
    (phrase(sentence(Result), Atoms) -> 
         format(string(Answer), "~w", [Result]);
     format(string(Answer), "Unable to parse ~w", [Atoms])).

:- add_answerer(lang:lang_answer).

%% lang.pl ends here
