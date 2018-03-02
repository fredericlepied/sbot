%% -*- prolog -*-

:- module(autoupdate, []).

:- use_module(config).
:- use_module(utils).
:- use_module(world).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fact updater
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

autoupdate_updater(_) :-
    autoupdate_code("."),
    findall(GitUrl, (config(external_git_module, GitUrl),
                     dirbase(_, Base, GitUrl),
                     format(string(Dir), "modules/~w", [Base]),
                     autoupdate_code(Dir)), _).

% store modules to be able to compare them to the next iteration
autoupdate_updater(Gen) :-
    config(modules, Modules),
    store_fact(Gen, modules(Modules)).

% load newly added module
autoupdate_updater(_) :-
    config(modules, Modules),
    member(Module, Modules),
    get_old_fact(modules(OldModules)),
    not(member(Module, OldModules)),
    writeln(["loading new module", Module]),
    use_module(Module).

% unload newly removed module
autoupdate_updater(_) :-
    get_old_fact(modules(OldModules)),
    member(Module, OldModules),
    config(modules, Modules),
    not(member(Module, Modules)),
    writeln(["unloading module", Module]),
    unload_file(Module).

% working directory is dirty
autoupdate_code(Dir) :-
    not(cmd("cd ~w && git diff --no-ext-diff --quiet --exit-code", [Dir])),
    make,
    !.

% working directory is clean, so update from remote with support for
% local commits not pushed
autoupdate_code(Dir) :-
    cmd("cd ~w && git fetch origin; git rebase origin/master", [Dir]),
    make.

:- add_fact_updater(autoupdate:autoupdate_updater).

%% autoupdate.pl ends here
