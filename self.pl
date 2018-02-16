%% -*- prolog -*-

:- module(self,
          [
          ]).

:- use_module(world).
:- use_module(utils).
:- use_module(discuss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% status predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

self_version(Version) :-
    cmd("git log --format=%h -1", [], [Version|_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% communication predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

self_answer(List, _, "version: display the version of my source code.") :-
    member("self", List),
    member("help", List).

self_answer(List, Nick, Answer) :-
    member("version", List),
    self_version(Version),
    format(string(Answer), "version ~w", [Version]).

:- add_answerer(self:self_answer).


%% self.pl ends here
