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

self_answer(Text, Nick, Answer) :-
    split_string(Text, " ", "", List),
    member("version", List),
    self_version(Version),
    format(atom(Answer), "~w: version ~w", [Nick, Version]).

:- add_answerer(self:self_answer).


%% self.pl ends here
