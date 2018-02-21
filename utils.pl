%% -*- prolog -*-

:- module(utils,
          [cmd/2, cmd/3, cmd/4, string_join/3, dirbase/3, workspace/1
          ]).


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

workspace(W) :-
    getenv("HOME", Home),
    string_concat(Home, "/workspace", W),
    mkdir(W).

mkdir(D) :-
    exists_directory(D), !.

mkdir(D) :-
    make_directory(D).

%% utils.pl ends here
