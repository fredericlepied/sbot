%% -*- prolog -*-

:- module(utils,
          [cmd/2, cmd/3, string_join/3
          ]).


cmd(Fmt, Args) :-
    format(atom(Cmd), Fmt, Args),
    string_concat("+ ", Cmd, StrCmd),
    writeln(StrCmd),
    shell(Cmd).

cmd(Fmt, Args, OutputLines) :-
    format(atom(Cmd), Fmt, Args),
    string_concat("+ ", Cmd, StrCmd),
    writeln(StrCmd),
    setup_call_cleanup(
        process_create(path(bash), ["-c", Cmd],
                       [stdout(pipe(Out))]),
        read_lines(Out, OutputLines),
        close(Out)).

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

%% utils.pl ends here