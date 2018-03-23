%% -*- prolog -*-

:- module(lang, [sentence/3]).

:- discontiguous noun/3.

:- use_module(discuss).
:- use_module(utils).
:- use_module(world).

sentence(V) --> polite, verbal_group(V), ["?"].
sentence(V) --> verbal_group(V).
sentence(find(O,A,C)) --> pronoun, object(O), be, action(A), object(C), ["?"].
sentence(find(O)) --> pronoun, be, object(O), ["?"].
sentence(find(O)) --> pronoun, object(O), interrogation_verb, ["?"].
sentence(find(O, C)) --> pronoun, object(O), interrogation_verb, complement(C), ["?"].
sentence(find(O, C)) --> pronoun, be, object(O), complement(C), ["?"].
sentence(count(O)) --> how_many, object(O), ["?"].
sentence(count(O)) --> how_many, object(O), interrogation_verb, ["?"].
sentence(count(O, C)) --> how_many, object(O), interrogation_verb, complement(C), ["?"], {validate_count(O,C)}.

how_many --> polite, ["how","many"].
how_many --> polite, ["count","how","many"].
how_many --> polite, ["count"].

interrogation_verb --> ["do","we","have"].
interrogation_verb --> ["have","we"].
interrogation_verb --> ["do","we","run"].
interrogation_verb --> ["do","we","possess"].

polite --> please, ["can","you"], please.
polite --> please, ["could","you"], please.
polite --> please.
polite --> [].

please --> ["please"].
please --> [].

pronoun --> ["what"].
pronoun --> ["which"].

be --> ["is"].
be --> ["are"].

det --> ["the"].
det --> [].

action(testing) --> ["testing"].

verbal_group(apply(O)) --> ["apply"], object(O).
verbal_group(build(O)) --> ["build"], object(O).
verbal_group(follow(O)) --> ["track"], object(O).
verbal_group(follow(O)) --> ["follow"], object(O).

object(obj(N)) --> det, noun(N).
object(obj(N, C)) --> det, noun(N, C).
object(obj(N, C)) --> det, noun(N), complement(C).

complement(N) --> ["in"], object(N).
complement(N) --> ["on"], object(N).
complement(N) --> ["for"], object(N).
complement(N) --> ["at"], object(N).

noun(pr(Pr)) --> github_pr, number(Pr).
noun(pr(_)) --> github_pr.
noun(issue(Issue)) --> github_issue, number(Issue).
noun(issue(_)) --> github_issue.
noun(review(Review)) --> gerrit_review, number(Review).
noun(review(_)) --> gerrit_review.
noun(project(Project)) --> project(Project), ["project"].
noun(project(Project)) --> ["project"], project(Project).
noun(project(Project)) --> project(Project).
noun(project(_)) --> ["project"].
noun(package(Package)) --> ["package"], package(Package).
noun(package(Package)) --> package(Package), ["package"].
noun(package(Package)) --> package(Package).
noun(package(_)) --> ["package"].
noun(job(Job)) --> job(Job).
noun(job(_)) --> ["job"].
noun(job(_)) --> ["jobs"].
noun(product(Product)) --> product(Product).
noun(product(Product)) --> product(Product), ["product"].
noun(product(Product)) --> ["product"], product(Product).
noun(product(_)) --> ["product"].
noun(product(_)) --> ["products"].
noun(component(Component)) --> component(Component).
noun(component(Component)) --> component(Component), ["component"].
noun(component(Component)) --> ["component"], component(Component).
noun(component(_)) --> ["component"].
noun(component(_)) --> ["components"].
noun(topic(Topic)) --> topic(Topic).
noun(partner(Partner)) --> partner(Partner).
noun(partner(_)) --> ["partner"].
noun(partner(_)) --> ["partners"].
noun(card(Card)) --> ["trello","card"], card(Card).
noun(card(Card)) --> ["card"], card(Card).
noun(card(_)) --> ["card"].
noun(card(_)) --> ["cards"].
noun(list(List)) --> ["trello","list"], list(List).
noun(list(List)) --> ["list"], list(List).
noun(list(List)) --> list(List), ["list"].
noun(list(List)) --> list(List).
noun(list(_)) --> ["trello","list"].
noun(list(_)) --> ["list"].
noun(list(_)) --> ["trello","lists"].
noun(list(_)) --> ["lists"].
noun(status(O)) --> ["status","of"], object(O).
noun(puddle(Puddle)) --> puddle(Puddle).
noun(puddle(Puddle)) --> puddle(Puddle), ["puddle"].
noun(puddle(Puddle)) --> ["puddle"], puddle(Puddle).
noun(puddle(_)) --> ["puddle"].
noun(puddle(_)) --> ["puddles"].
noun(alias(_)) --> ["alias"].
noun(alias(_)) --> ["aliases"].
noun(puddle(_),alias(Alias)) --> alias(Alias), ["puddle"].
noun(puddle(_),alias(Alias)) --> alias(Alias).

github_pr --> ["pr"].
github_pr --> ["github","pr"].

github_issue --> ["issue"].
github_issue --> ["github","issue"].

gerrit_review --> ["review"].
gerrit_review --> ["gerrit","review"].
gerrit_review --> ["reviews"].
gerrit_review --> ["gerrit","reviews"].

number(42, ["42"|R], R).
number(42, [42|R], R).

number(I, [S|R], R) :-
    number_string(I, S).

project("ansible", ["ansible"|R], R).
project("systemd", ["systemd"|R], R).
project(P, [P|R], R) :-
    is_a(project, P).

package("ansible", ["ansible"|R], R).

job(Topic) --> topic(Topic), ["job"].

topic(T, [T|R], R) :-
    is_a(job, T).

product(T, [T|R], R) :-
    is_a(product, T).

component(T, [T|R], R) :-
    is_a(component, T).

card(C, [C|R], R) :-
    is_a(card, C).

card(C, L, R) :-
    is_a(card, C),
    split_string(C, " ", "", Strings),
    append(Strings, R, L).

list(C, [C|R], R) :-
    is_a(list, C).

list(C, L, R) :-
    is_a(list, C),
    split_string(C, " ", "", Strings),
    append(Strings, R, L).

partner(P, [P|R], R) :-
    is_a(partner, P).

validate_count(A, B) :-
    validate_apply(A, B).
validate_count(partner(_), topic(_)).

validate_apply(pr(_), package(_)).
validate_apply(review(_), package(_)).

puddle(C, [C|R], R) :-
    is_a(puddle, C).

alias(T, [T|R], R) :-
    is_a(alias, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_a(review, O) :-
    get_fact(gerrit_open_review(_,O,_,_,_)).

is_a(project, O) :-
    get_fact(gerrit_open_review(O, _,_,_,_)).

is_a(job, O) :-
    get_fact(dci_job(_, _, _, _, O, _, _, _)).

is_a(product, O) :-
    get_fact(dci_component(O, _, _)).

is_a(component, O) :-
    get_fact(dci_component(_, O, _)).

is_a(card, O) :-
    get_fact(trello_card(_, O, _, _, _, _, _, _, _, _, _)).

is_a(list, E) :-
    get_fact(trello_list(_, E, _, _, _)).

is_a(alert, O) :-
    get_fact(prometheus_alert(_, Alert)),
    O = Alert.annotations.summary.

is_a(puddle, O) :-
    get_fact(puddle_info(_, _, _, O)).

is_a(alias, O) :-
    get_fact(puddle_info(_, _, O, _)).

property(O, list, P) :-
    get_fact(trello_card(_, O, _, _, _, _, _, _, P, _, _)).

property(O, project, P) :-
    get_fact(gerrit_open_review(P ,O, _, _, _)).

property(O, id, P) :-
    get_fact(gerrit_open_review(_, O, P, _, _)).

property(O, owner, P) :-
    get_fact(gerrit_open_review(_ , O, P, _, _)).

property(O, subject, P) :-
    get_fact(gerrit_open_review(_ , O, _, P, _)).

property(O, update, P) :-
    get_fact(gerrit_open_review(_ , O, _, _, P)).

property(O, product, P) :-
    get_fact(dci_job(P, _, _, _, O, _, _, _)).

property(O, component, P) :-
    get_fact(dci_job(_, P, _, _, O, _, _, _)).

% product of a component
property(O, product, P) :-
    get_fact(dci_job(P, O, _, _, _, _, _, _)).

property(O, topic, P) :-
    get_fact(dci_job(_, _, String, _, O, _, _, _)),
    string_lower(String, Lower),
    atom_string(Lower, P).

property(O, partner, P) :-
    get_fact(dci_job(_, _, _, P, O, _, _, _)).

property(O, status, P) :-
    get_fact(dci_job(_, _, _, _, O, P, _, _)).

property(O, id, P) :-
    get_fact(dci_job(_, _, _, _, O, _, P, _)).

property(O, rconf, P) :-
    get_fact(dci_job(_, _, _, _, O, _, _, P)).

property(O, severity, P) :-
    get_fact(prometheus_alert(_, Alert)),
    O = Alert.annotations.summary,
    P = Alert.labels.severity.

property(O, instance, P) :-
    get_fact(prometheus_alert(_, Alert)),
    O = Alert.annotations.summary,
    P = Alert.labels.instance.

property(O, component, P) :-
    get_fact(puddle_info(P, _, _, O)).

property(O, url, P) :-
    get_fact(puddle_info(_, P, _, O)).

property(O, type, P) :-
    get_fact(puddle_info(_, _, P, O)).

property(O, alias, P) :-
    get_fact(puddle_info(_, _, P, O)).

property(O, component, P) :-
    get_fact(puddle_info(P, _, O, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NLU: Compute answer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute(count(obj(Obj)), Answer) :-
    compound_name_arity(Obj, Name, _),
    setof(Var, is_a(Name, Var), List),
    length(List, Len),
    format(string(Answer), "we have ~w ~ws", [Len, Name]),
    writeln(count(Obj, Len)).

% count(obj(job(_1128),obj(topic(osp13))))
execute(count(obj(Obj, obj(Obj2))), Answer) :-
    compound_name_arity(Obj, Name, _),
    compound_name_arguments(Obj2, Name2, [Prop]),
    setof(Var, (is_a(Name, Var), property(Var, Name2, Prop)), List),
    length(List, Len),
    format(string(Answer), "we have ~w ~ws for ~w", [Len, Name, Prop]).

execute(count(obj(Obj, obj(Obj2))), Answer) :-
    compound_name_arity(Obj, Name, _),    
    compound_name_arguments(Obj2, _, [Prop]),
    is_a(Name, _),
    format(string(Answer), "we have no ~w for ~w", [Name, Prop]).

execute(find(obj(Obj)), Answer) :-
    compound_name_arity(Obj, Name, _),
    setof(Var, is_a(Name, Var), List),
    string_join(", ", List, Answer). 

execute(find(obj(Obj), obj(Obj2)), Answer) :-
    compound_name_arity(Obj, Name, _),
    compound_name_arguments(Obj2, Name2, [Prop]),
    setof(Var, (is_a(Name, Var), property(Var, Name2, Prop)), List),
    string_join(", ", List, Answer). 

execute(find(obj(Obj, obj(Obj2))), Answer) :-
    execute(find(obj(Obj), obj(Obj2)), Answer).

execute(find(obj(Obj), obj(Obj2)), Answer) :-
    compound_name_arity(Obj, Name, _),
    compound_name_arguments(Obj2, _, [Prop]),
    is_a(Name, _),
    format(string(Answer), "we have no ~w for ~w", [Name, Prop]).

% find(obj(puddle(_1016),alias(latest)),obj(component(OSP11)))
execute(find(obj(Obj, Obj2), obj(Obj3)), Answer) :-
    compound_name_arity(Obj, Name, _),
    compound_name_arguments(Obj2, Name2, [Prop2]),
    compound_name_arguments(Obj3, Name3, [Prop3]),
    writeln(setof(Var, (is_a(Name, Var), property(Var, Name2, Prop2), property(Var, Name3, Prop3)), List)),
    setof(Var, (is_a(Name, Var), property(Var, Name2, Prop2), property(Var, Name3, Prop3)), List),
    string_join(", ", List, Answer). 

execute(find(obj(_)), "Unable to find what you asked for. Sorry.").

execute(find(obj(_), _, obj(_)), "Unable to find what you asked for. Sorry.").

execute(F, "No entiendo. Sorry.") :-
    writeln(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strings_atoms(ListOfStrings, ListOfAtoms) :-
    % map(string_lower, ListOfStrings, Lowers),
    map(string_atom, ListOfStrings, ListOfAtoms).
    
lang_answer(["lang"|List], _, Answer) :-
    (phrase(sentence(Result), List) -> 
         (writeln(execute(Result)), execute(Result, Answer));
     format(string(Answer), "Unable to understand ~w", [List])).

:- add_answerer(lang:lang_answer).

%% lang.pl ends here
