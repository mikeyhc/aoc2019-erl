-module(painting_robot).

-export([run_robot/1, render_robot/1]).

run_robot(Intcode) ->
    run_robot(intcode:intcode_status(Intcode), Intcode, up, {0, 0}, #{}).

run_robot(halted, Intcode, _Direction, _Pos, Map) -> {Intcode, Map};
run_robot(_Status, Intcode0, Direction, Pos, Map) ->
    Tile = maps:get(Pos, Map, 0),
    Intcode1 = intcode:add_input(Tile, Intcode0),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {[Paint, Dir], Intcode3} = intcode:pop_all_output(Intcode2),
    NewMap = maps:put(Pos, Paint, Map),
    NewDir = turn(Direction, Dir),
    NewPos = move(NewDir, Pos),
    run_robot(intcode:intcode_status(Intcode3), Intcode3, NewDir, NewPos,
              NewMap).

render_robot(Intcode) ->
    {_Intcode, Map} = run_robot(intcode:intcode_status(Intcode), Intcode,
                                up, {0, 0}, #{{0, 0} => 1}),
    Keys = maps:keys(Map),
    {Xs, Ys} = lists:unzip(Keys),
    MinX = lists:min(Xs),
    MinY = lists:min(Ys),
    render(MinX, MinY, maps:to_list(Map)).

turn(up, 0) -> left;
turn(up, 1) -> right;
turn(down, 0) -> right;
turn(down, 1) -> left;
turn(left, 0) -> down;
turn(left, 1) -> up;
turn(right, 0) -> up;
turn(right, 1) -> down.

move(up, {X, Y}) -> {X, Y - 1};
move(down, {X, Y}) -> {X, Y + 1};
move(left, {X, Y}) -> {X - 1, Y};
move(right, {X, Y}) -> {X + 1, Y}.

group_rows(Points) ->
    M = fun(Key, Val, Map) ->
                Old = maps:get(Key, Map, []),
                maps:put(Key, [Val|Old], Map)
        end,
    F = fun({{X, Y}, V}, Acc) -> M(Y, {X, V}, Acc) end,
    lists:sort(maps:to_list(lists:foldl(F, #{}, Points))).

normalize_coords(BaseX, BaseY, Points) ->
    lists:map(fun({{X, Y}, V}) -> {{X - BaseX, Y - BaseY}, V} end, Points).

render(BaseX, BaseY, Points) ->
    Normalized = normalize_coords(BaseX, BaseY, Points),
    Rows = group_rows(Normalized),
    render_rows(Rows).

render_blank_rows(Count) ->
    lists:foreach(fun(_) -> io:format("~n") end, lists:seq(0, Count - 1)).

render_rows(Rows) ->
    F = fun({I, Row}, Idx) ->
                render_blank_rows(I - Idx),
                render_row(Row),
                I + 1
        end,
    lists:foldl(F, 0, Rows),
    ok.

render_blank_pixels(Count) ->
    lists:foreach(fun(_) -> io:format(" ") end, lists:seq(0, Count - 1)).

render_row(Row) ->
    Pixels = lists:keysort(1, Row),
    F = fun({I, P}, Idx) ->
                render_blank_pixels(I - Idx),
                case P of
                    0 -> io:format(" ");
                    1 -> io:format("#")
                end,
                I + 1
        end,
    lists:foldl(F, 0, Pixels),
    io:format("~n"),
    ok.
