-module(ascii).

-export([intersection_distances/1, count_dust/1, render_scaffold/1]).


%%% part 1

intersection_distances(Intcode) ->
    Intersections = find_intersections(Intcode),
    lists:sum(lists:map(fun({A, B}) -> A * B end, Intersections)).

to_coords(Intcode) ->
    {ok, I0} = intcode:run_intcode(Intcode),
    {Output, _} = intcode:pop_all_output(I0),
    map_to_coords(Output).

find_intersections(Intcode) ->
    Coords = to_coords(Intcode),
    intersections(Coords).

map_to_coords(Input) ->
    Rows = to_rows(Input),
    lists:foldl(fun row_coords/2, #{},
                lists:zip(lists:seq(0, length(Rows) - 1), Rows)).

row_coords({Idx, Row}, Output) ->
    F = fun({_, $.}, O) -> O;
           ({Col, C}, O) -> O#{{Col, Idx} => C}
        end,
    lists:foldl(F, Output, lists:zip(lists:seq(0, length(Row) - 1), Row)).

to_rows(L) -> to_rows(L, [[]]).

to_rows([], Acc) -> lists:reverse(Acc);
to_rows([$\n], Acc) -> lists:reverse(Acc);
to_rows([$\n|T], [AH|AT]) ->
    to_rows(T, [[],lists:reverse(AH)|AT]);
to_rows([H|T], [AH|AT]) ->
    to_rows(T, [[H|AH]|AT]).

intersections(Coords) ->
    F = fun({_, $.}, Acc) -> Acc;
           ({{X, Y}, _}, Acc) ->
                case intersects({X, Y}, Coords) of
                    true -> [{X, Y}|Acc];
                    false -> Acc
                end
        end,
    lists:foldl(F, [], maps:to_list(Coords)).

valid_scaffold($#) -> true;
valid_scaffold($<) -> true;
valid_scaffold($^) -> true;
valid_scaffold($>) -> true;
valid_scaffold($v) -> true;
valid_scaffold(_) -> false.

intersects({X, Y}, Map) ->
    V = [valid_scaffold(maps:get({X - 1, Y}, Map, $.)),
         valid_scaffold(maps:get({X + 1, Y}, Map, $.)),
         valid_scaffold(maps:get({X, Y - 1}, Map, $.)),
         valid_scaffold(maps:get({X, Y + 1}, Map, $.))],
    lists:foldl(fun(A, B) -> A and B end, true, V).


%%% part 2

count_dust(Intcode) ->
    Coords = to_coords(Intcode),
    Path = find_path(Coords),
    Subpaths = {A, B, C} = find_subpaths(Path),
    Main = build_main_movement(Path, Subpaths),
    Input = build_input(Main, [A, B, C]),
    I0 = intcode:set_instruction(0, 2, Intcode),
    I1 = intcode:set_input(Input, I0),
    {ok, I2} = intcode:run_intcode(I1),
    {Output, _} = intcode:pop_all_output(I2),
    [Value|_] = lists:reverse(Output),
    Value.

find_path(Map) ->
    {X, Y} = find_start(Map),
    find_path({X, Y+1}, {X, Y}, north, Map, [start]).

find_start(Map) ->
    List = maps:to_list(Map),
    {Start, _} = lists:keyfind($^, 2, List),
    Start.

find_path(Previous, Current, Direction, Map, D=[Last|T]) ->
    Next = find_next(Current, Direction),
    case maps:is_key(Next, Map) of
        true -> find_path(Current, Next, Direction, Map, [Last+1|T]);
        false ->
            case find_turn(Current, Previous, Direction, Map) of
                false ->
                    [_|R] = lists:reverse(D),
                    R;
                {Turn, N} ->
                    NewDirection = turn_to_direction(Direction, Turn),
                    find_path(Current, N, NewDirection, Map, [1,Turn|D])
            end
    end.

find_next({X, Y}, north) -> {X, Y-1};
find_next({X, Y}, east) -> {X+1, Y};
find_next({X, Y}, south) -> {X, Y+1};
find_next({X, Y}, west) -> {X-1, Y}.

find_turn({X, Y}, Previous, Direction, Map) ->
    Dirs = [{X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}],
    TrueDirs = lists:delete(Previous, Dirs),
    F = fun(Key, C={_, _}) ->
                case maps:is_key(Key, Map) of
                    true -> throw(invalid_state);
                    false -> C
                end;
           (Key, false) ->
                case maps:is_key(Key, Map) of
                    true -> Key;
                    false -> false
                end
        end,
    case lists:foldl(F, false, TrueDirs) of
        false -> false;
        C={X0, Y0} ->
            Turn = coords_to_turn(Direction, {X-X0, Y-Y0}),
            {Turn, C}
    end.

coords_to_turn(north, {-1, 0}) -> $R;
coords_to_turn(north, {1, 0}) -> $L;
coords_to_turn(east, {0, -1}) -> $R;
coords_to_turn(east, {0, 1}) -> $L;
coords_to_turn(south, {-1, 0}) -> $L;
coords_to_turn(south, {1, 0}) -> $R;
coords_to_turn(west, {0, -1}) -> $L;
coords_to_turn(west, {0, 1}) -> $R.

turn_to_direction(north, $L) -> west;
turn_to_direction(north, $R) -> east;
turn_to_direction(east, $L) -> north;
turn_to_direction(east, $R) -> south;
turn_to_direction(south, $L) -> east;
turn_to_direction(south, $R) -> west;
turn_to_direction(west, $L) -> south;
turn_to_direction(west, $R) -> north.

build_subpaths(List) ->
    build_subpaths(List, []).

build_subpaths([], Out) -> lists:map(fun lists:reverse/1, Out);
build_subpaths(L=[_,_|T], Out) ->
    Length = length(L),
    Dist = min(20, Length),
    P = lists:reverse(lists:sublist(L, Dist)),
    NewOut = build_path_(P, Out),
    Subpaths = build_subpaths(T, [P|NewOut]),
    sets:to_list(sets:from_list(Subpaths)).

build_path_([], Out) -> Out;
build_path_([_,_], Out) -> Out;
build_path_([_,_|T], Out) ->
    build_path_(T, [T|Out]).

find_subpaths(Path) ->
    Subpaths = build_subpaths(Path),
    find_combination(Path, Subpaths).

find_combination(_Path, []) -> throw(no_valid_combination);
find_combination(Path, [A|T]) ->
    Parts = split_path(Path, A),
    case fill_remaining(Parts, T) of
        {B, C} -> {A, B, C};
        false -> find_combination(Path, T)
    end.

split_path(Path, Subpath) ->
    split_path(Path, Subpath, [[]]).

split_path([], _Subpath, Out) -> lists:map(fun lists:reverse/1, Out);
split_path(Path=[H|T], Subpath, Out=[OH|OT]) ->
    case match_subpath(Path, Subpath) of
        {match, Rest} -> split_path(Rest, Subpath, [[]|Out]);
        nomatch -> split_path(T, Subpath, [[H|OH]|OT])
    end.

match_subpath(Rest, []) -> {match, Rest};
match_subpath([H|T], [H|Rest]) ->
    match_subpath(T, Rest);
match_subpath(_, _) -> nomatch.

fill_remaining(_Parts, []) -> false;
fill_remaining(Parts, [H|T]) ->
    Remaining = unwrap(lists:map(fun(X) -> split_path(X, H) end, Parts)),
    case lists:search(fun(X) -> fit_subpath(Remaining, X) end, T) of
        {value, Value} -> {H, Value};
        false -> fill_remaining(Parts, T)
    end.

unwrap(L0) ->
    L1 = lists:foldl(fun(X, Acc) -> X ++ Acc end, [], L0),
    lists:filter(fun(X) -> X =/= [] end, L1).

fit_subpath(Parts, Subpath) ->
    unwrap(lists:map(fun(X) -> split_path(X, Subpath) end, Parts)) == [].

build_main_movement(Path, {A, B, C}) ->
    build_main_movement(Path, A, B, C, []).

build_main_movement([], _, _, _, Acc) -> intersperse(lists:reverse(Acc), $,);
build_main_movement(Path, A, B, C, Acc) ->
    {V, NewPath} = match_prefix(Path, [{A, $A}, {B, $B}, {C, $C}]),
    build_main_movement(NewPath, A, B, C, [V|Acc]).

match_prefix(_Path, []) -> throw(invalid_state);
match_prefix(Path, [{P, V}|T]) ->
    case lists:prefix(P, Path) of
        true -> {V, drop(length(P), Path)};
        false -> match_prefix(Path, T)
    end.

drop(0, L) -> L;
drop(N, [_|T]) -> drop(N - 1, T).

intersperse(L, C) -> intersperse(L, C, []).

intersperse([], _, Acc) -> lists:reverse(Acc);
intersperse([X], _, Acc) -> lists:reverse([X|Acc]);
intersperse([H|T], C, Acc) ->
    intersperse(T, C, [C,H|Acc]).

build_input(Current, []) -> Current ++ "\nn\n";
build_input(Current, [H|T]) ->
    String = instruction_to_string(H),
    build_input(Current ++ "\n" ++ String, T).

instruction_to_string([]) -> [];
instruction_to_string([C,N]) ->
    [C] ++ "," ++ integer_to_list(N);
instruction_to_string([C,N|T]) ->
    [C] ++ "," ++ integer_to_list(N) ++ "," ++ instruction_to_string(T).

%%% draw map

render_scaffold(Intcode) ->
    Coords = to_coords(Intcode),
    Intersections = find_intersections(Intcode),
    render_scaffold(Coords, Intersections).

render_scaffold(Map, Intersections) ->
    Rows = maps:to_list(Map),
    RenderRows = build_render_rows(Rows),
    lists:foreach(fun(X) -> render_row(X, Intersections) end, RenderRows).

render_row({Row, Values}, Intersections) ->
    R0 = lists:keysort(1, Values),
    F = fun({R, C}, N) ->
                Dots = lists:duplicate(R - N, $.),
                C0 = case lists:member({R, Row}, Intersections) of
                         true -> $O;
                         false -> C
                     end,
                io:format("~s~c", [Dots, C0]),
                R + 1
        end, lists:foldl(F, 0, R0),
    io:format("~n").

build_render_rows(Rows) ->
    L = lists:foldl(fun({{X, Y}, C}, Acc) ->
                            P = maps:get(Y, Acc, []),
                            Acc#{Y => [{X, C}|P]}
                    end, #{}, Rows),
    lists:keysort(1, maps:to_list(L)).
