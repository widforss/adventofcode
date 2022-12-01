:- initialization main.
:- dynamic contain/3.

/* The predicate describing the problem. */
hold(Outer, N, Inner) :-
    contain(Outer, N, Inner).
hold(Outer, N, Inner) :-
    bagof(
        N_Int * N_Inn,
        Intermediate^(
            contain(Outer, N_Int, Intermediate),
            hold(Intermediate, N_Inn, Inner)),
        Ns),
    sumlist(Ns, N).

/* Main predicate */
main :-
    load,

    setof(Outer, N_1^hold(Outer, N_1, 'shiny gold bag'), Set_1),
    length(Set_1, Part_1_Result),
    format('Part 1: ~d.\n', [Part_1_Result]),

    bagof(N_2, Inner^hold('shiny gold bag', N_2, Inner), Bag_2),
    sumlist(Bag_2, Part_2_Result),
    format('Part 2: ~d.\n', [Part_2_Result]),
    halt.
main :-
    halt(1).

/* Procedures for loading predicates from list. */
load :-
    current_prolog_flag(argv, Argv),
    (length(Argv, 1) ->
        [File] = Argv;
        write('Exactly one argument after -- required\n'), fail),

    catch(
        setup_call_cleanup(
            open(File, read, Stream),
            load_loop(Stream),
            close(Stream)),
        _,
        (format('Failed to read file: ~s\n', [File]), fail)).

load_loop(Stream) :-
    read_line_to_string(Stream, Str),
    (Str \= end_of_file ->
        atomic_list_concat([Bag_Str, N_Inner_Bags_Str], "s contain", Str),
        split_string(N_Inner_Bags_Str, ",", ",. ", N_Inner_Bags_Lst),
        atom_string(Bag, Bag_Str),
        add_relation(Bag, N_Inner_Bags_Lst),
        load_loop(Stream);
        true).

add_relation(_, []).
add_relation(Bag, [N_Inner_Bag_Str|N_Inner_Bags_Lst]) :-
    sub_string(N_Inner_Bag_Str, 0, 1, _, N_Str),
    sub_string(N_Inner_Bag_Str, 2, _, 0, Inner_Bag_Str),
    rm_trailing_s(Inner_Bag_Str, Inner_Bag_Stripped_Str),
    atom_string(Inner_Bag, Inner_Bag_Stripped_Str),
    atom_string(N_Atom, N_Str),
    atom_number(N_Atom, N),
    assertz(contain(Bag, N, Inner_Bag)),
    add_relation(Bag, N_Inner_Bags_Lst).

rm_trailing_s(Str, Stripped_Str) :-
    string_concat(Stripped_Str, "s", Str);
    Str = Stripped_Str.