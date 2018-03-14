%% -*- prolog -*-

:- module(utils,
          [cmd/2, cmd/3, cmd/4, string_join/3, dirbase/3, workspace/2, url_workspace/2, get_config/3,
           load_external_modules/0, sublist/2, map/3, string_atom/2
          ]).

:- use_module(config).

get_config(Key, Value, _) :-
    config(Key, Value),
    !.

get_config(_, Value, Value).

cmd(Fmt, Args) :-
    format(string(Cmd), Fmt, Args),
    string_concat("+ ", Cmd, StrCmd),
    writeln(StrCmd),
    shell(Cmd, Status),
    !,
    Status == 0.

cmd(Fmt, Args, OutputLines) :-
    cmd(Fmt, Args, OutputLines, 0).

cmd(Fmt, Args, OutputLines, Status) :-
    format(string(Cmd), Fmt, Args),
    string_concat("+ ", Cmd, StrCmd),
    writeln(StrCmd),
    process_create(path(bash), ["-c", Cmd],
                   [stdout(pipe(Out)),stderr(pipe(Out)),process(Pid)]),
    read_lines(Out, OutputLines),
    close(Out),
    !,
    process_wait(Pid, exit(Status)).

read_lines(Out, Lines) :-
        read_line_to_codes(Out, Line1),
        read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.

read_lines(Codes, Out, [Line|Lines]) :-
        atom_codes(Line, Codes),
        read_line_to_codes(Out, Line2),
        read_lines(Line2, Out, Lines).

string_join(_, [], "").

string_join(_, [Single], Single) :-
    !.

string_join(Sep, [First|Rest], Res) :-
    string_concat(First, Sep, FirstSep),
    string_join(Sep, Rest, RestTxt),
    string_concat(FirstSep, RestTxt, Res),
    !.

dirbase(Dir, Base, Path) :-
    split_string(Path, "/", "", L),
    reverse(L, [Base|R]),
    reverse(R, RR),
    string_join("/", RR, Dir).

url_workspace(Module, Url) :-
    (config(workspace_url, WsUrl) -> true; 
     (getenv("USER", User),
      format(string(WsUrl), "http://localhost/~~~w/", [User]))),
    format(string(Url), "~w~w/", [WsUrl, Module]).

workspace(Module, W) :-
    (config(workspace, Ws) -> true; 
     (getenv("HOME", Home),
      format(string(Ws), "~w/public_html", [Home]))),
    format(string(W), "~w/~w", [Ws, Module]),
    mkdir(W).

mkdir(D) :-
    exists_directory(D), !.

mkdir(D) :-
    dirbase(Dir, _, D),
    mkdir(Dir),
    make_directory(D).

load_external_modules :-
    findall(Url, (config(external_git_module, Url),
                  load_external_module(Url)), _).

load_external_module(GitUrl) :-
    cmd("extract_module.sh ~w", [GitUrl]),
    dirbase(_, Base, GitUrl),
    format(string(Path), "modules/~w", [Base]),
    export_tools_path(Path),
    file_search_path(swi, Path).

export_tools_path(RelPath) :-
    absolute_file_name(RelPath, Abs),
    format(string(ToolsDir), "~w/tools", [Abs]),
    exists_directory(ToolsDir),
    getenv("PATH", Paths),
    split_string(Paths, ":", "", List),
    string_join(":", [ToolsDir|List], NewPaths),
    setenv("PATH", NewPaths),
    writeln(setenv("PATH", NewPaths)).

export_tools_path(_).

sublist([], _).
sublist([X|XS], [X|XSS]) :- sublist(XS, XSS), !.
sublist(["*"|XS], [_|XSS]) :- sublist(XS, XSS).

map(_, [], []) :- !.
map(F, [S|Rest], [R|RRest]) :-
    call(F, S, R),
    map(F, Rest, RRest).

string_atom(S, A) :-
    atom_string(A, S).

%% utils.pl ends here
