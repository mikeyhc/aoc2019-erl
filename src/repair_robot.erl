-module(repair_robot).

-export([run_robot/1, run_robot_full/1, commands_to_oxygen/1,
         oxygen_spread/1, draw_map/1]).

%%% Part 1

run_robot(Intcode) ->
    run_robot({0, 0}, Intcode, sets:new()).

commands_to_oxygen(Intcode) ->
    length(run_robot(Intcode)) - 1.

run_robot(Pos, Intcode, Nodes) ->
    NewNodes = sets:add_element(Pos, Nodes),
    Neighbours = get_neighbours(Pos),
    NewNeighbours = sets:subtract(Neighbours, Nodes),
    case find_paths(Pos, NewNeighbours, Intcode, NewNodes) of
        N={nothing, _} -> N;
        L=[_|_] -> [Pos|L]
    end.

get_neighbours({X, Y}) ->
    sets:from_list([{X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}]).

find_paths(Pos, Neighbours, Intcode, Nodes) ->
    DiffFun = fun(X) -> {X, diff_to_command(Pos, X)} end,
    Commands = lists:map(DiffFun, sets:to_list(Neighbours)),
    Outputs = lists:map(fun(X) -> run_intcode(Intcode, X) end, Commands),
    find_paths_(Outputs, Nodes).

run_intcode(Intcode0, {Pos, Input}) ->
    Intcode1 = intcode:set_input([Input], Intcode0),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {[Output], Intcode3} = intcode:pop_all_output(Intcode2),
    {Pos, Output, Intcode3}.

find_paths_([], Nodes) -> {nothing, Nodes};
find_paths_([{Pos, 2, _Intcode}|_], _Nodes) -> [Pos];
find_paths_([{Pos, 0, _Intcode}|T], Nodes) ->
    find_paths_(T, sets:add_element(Pos, Nodes));
find_paths_([{Pos, 1, Intcode}|T], Nodes) ->
    case run_robot(Pos, Intcode, Nodes) of
        {nothing, N} -> find_paths_(T, N);
        L -> L
    end.

diff_to_command({X0, _}, {X1, _}) when X0 < X1 -> 4;
diff_to_command({X0, _}, {X1, _}) when X0 > X1 -> 3;
diff_to_command({_, Y0}, {_, Y1}) when Y0 > Y1 -> 2;
diff_to_command(_, _) -> 1.

%%% Part 2

run_robot_full(Intcode) ->
    run_robot_full({0, 0}, Intcode, #{{0, 0} => 3}).

run_robot_full(Pos, Intcode, Nodes) ->
    F = fun(X) -> not maps:is_key(X, Nodes) end,
    Neighbours = get_neighbours(Pos),
    NewNeighbours = sets:filter(F, Neighbours),
    find_full_paths(Pos, NewNeighbours, Intcode, Nodes).

find_full_paths(Pos, Neighbours, Intcode, Nodes) ->
    DiffFun = fun(X) -> {X, diff_to_command(Pos, X)} end,
    Commands = lists:map(DiffFun, sets:to_list(Neighbours)),
    Outputs = lists:map(fun(X) -> run_intcode(Intcode, X) end, Commands),
    find_full_paths_(Outputs, Nodes).

find_full_paths_([], Nodes) -> Nodes;
find_full_paths_([{Pos, 0, _Intcode}|T], Nodes) ->
    find_full_paths_(T, Nodes#{Pos => 0});
find_full_paths_([{Pos, Space, Intcode}|T], Nodes0) ->
    Nodes1 = Nodes0#{Pos => Space},
    Nodes2 = run_robot_full(Pos, Intcode, Nodes1),
    find_full_paths_(T, Nodes2).

oxygen_spread(Map) ->
    MapList = maps:to_list(Map),
    {Start, _} = lists:keyfind(2, 2, MapList),
    oxygen_spread_([Start], 0, Map).

oxygen_spread_([], Steps, Map) -> {Steps - 1, Map};
oxygen_spread_(EdgeSet, Steps, Map0) ->
    Map1 = lists:foldl(fun(K, M) -> maps:update(K, 4, M) end, Map0, EdgeSet),
    % draw_map(Map1),
    % timer:sleep(1000),
    F = fun(X) -> maps:get(X, Map1, 0) =:= 1 end,
    Neighbours = lists:map(fun get_neighbours/1, EdgeSet),
    AllNeighbours = sets:union(Neighbours),
    NewNeighbours = sets:filter(F, AllNeighbours),
    oxygen_spread_(sets:to_list(NewNeighbours), Steps + 1, Map1).

%% Drawing

draw_map(Map) ->
    ListMap = maps:to_list(Map),
    Coords = lists:map(fun({X, _}) -> X end, ListMap),
    {Xs, Ys} = lists:unzip(Coords),
    XOffset = lists:min(Xs),
    YOffset = lists:min(Ys),
    Rows = render_sort(group_by_y(ListMap)),
    draw_rows(XOffset, YOffset, Rows),
    ok.

group_by_y(Coords) ->
    F = fun({{X, Y}, S}, Acc) ->
                maps:update_with(Y, fun(V) -> [{X, S}|V] end, [{X, S}], Acc)
        end,
    maps:to_list(lists:foldl(F, #{}, Coords)).

render_sort(Rows) ->
    NewRows = lists:map(fun({Y, C}) -> {Y, lists:keysort(1, C)} end, Rows),
    lists:keysort(1, NewRows).

draw_rows(XOffset, YOffset, Rows) ->
    Render = fun({Y, Cols}, Pos) ->
                     Diff = abs(Pos - Y),
                     io:format(lists:duplicate(Diff, $\n)),
                     render_row(XOffset, Cols),
                     Y + 1
             end,
    lists:foldl(Render, YOffset, Rows).

render_row(XOffset, Cols) ->
    Render = fun({X, S}, Pos) ->
                     Diff = abs(Pos - X),
                     io:format(lists:duplicate(Diff, $ )),
                     case S of
                         0 -> io:format("#");
                         1 -> io:format(".");
                         2 -> io:format("X");
                         3 -> io:format("d");
                         4 -> io:format("O")
                     end,
                     X + 1
             end,
    lists:foldl(Render, XOffset, Cols),
    io:format("\n").
