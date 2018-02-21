%% -*- prolog -*-

:- module(fedora, []).

:- use_module(discuss).
:- use_module(utils).
:- use_module(world).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact deducer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deduce_fedora_facts(_) :-
    get_longterm_fact(fedora_update_package(Pkg, Version, Context)),
    workspace(Ws),
    get_user(User),
    (cmd("fedora_update_pkg.sh '~w' ~w/fedora ~w ~w", [User, Ws, Pkg, Version]) ->
         Status = success;
     Status = failure),
    format(string(Text), "Updated package ~w to ~w: ~w", [Pkg, Version, Status]),
    remove_longterm_fact(fedora_update_package(Pkg, Version, Context)),
    notify(Text, Context).

get_user(User) :-
    config(fedora_user, User),
    !.

get_user("").

:- add_fact_deducer(fedora:deduce_fedora_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fedora help
fedora_answer(List, _, "fedora update <package> to <version>") :-
    member("fedora", List),
    member("help", List).

% fedora update ansible 2.5
fedora_answer(["fedora", "update", Pkg, "to", Version], Context, Answer) :-
    store_longterm_fact(fedora_update_package(Pkg, Version, Context)),
    format(string(Answer), "Added update fedora package ~w to ~w to my backlog.",
           [Pkg, Version]).

:- add_answerer(fedora:fedora_answer).

%% fedora.pl ends here
