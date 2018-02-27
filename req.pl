%% -*- prolog -*-

:- module(req, [req_pkg/1, pkg_or_abort/1]).

:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

req_pkg(Name) :-
    cmd("rpm -q ~w", [Name]),
    !.

req_pkg(Name) :-
    cmd("sudo -n dnf install -y ~w", [Name]).

pkg_or_abort(Name) :-
    req_pkg(Name) -> true; abort.

%% req.pl ends here
