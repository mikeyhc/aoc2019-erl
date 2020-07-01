-module(repair_robot).

-export([run_robot/1]).

-compile(export_all).

run_robot(Intcode) ->
    Intcode,
    run_robot(Intcode, {0, 0}, new_directions({0, 0}, #{}), #{{0, 0} => floor}).

run_robot(_Intcode, _Current, [], Map) -> Map;
run_robot(Intcode0, Current, [Next|T], Map) ->
    Path = find_path(Current, Next, Map),
    Instruction = to_instructions(Path ++ [Next]),
    Intcode1 = intcode:set_input(Instruction, Intcode0),
    NewDirections = new_directions(Current, Map),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {[Output], Intcode3} = intcode:pop_all_output(Intcode2),
    Space = num_to_space(Output),
    Pos = case Space of
              wall -> Current;
              floor -> Next;
              oxygen -> Next
          end,
    run_robot(Intcode3, Pos, NewDirections ++ T, Map#{Next => Space}).

new_directions({X, Y}, Map) ->
    L = [{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}],
    lists:filter(fun(P) -> not maps:is_key(P, Map) end, L).

num_to_space(0) -> wall;
num_to_space(1) -> floor;
num_to_space(2) -> oxygen.

min_path_cost({X0, Y0}, {X1, Y1}) ->
    abs(X0 - X1) + abs(Y0 - Y1).

find_path(Current, Next, Nodes) ->
    find_path([{Current, min_path_cost(Current, Next)}], Next, #{}, #{}, #{},
              Nodes).

find_path([], _Target, _GScore, _FScore, _Previous, _Nodes) ->
    throw(invalid_state);
find_path([{H, _}|T], Target, GScore, FScore, Previous, Nodes) ->
    case is_goal(H, Target) of
        true ->
            build_path(Previous, H) ++ [H];
        false ->
            {_, _, N, G, F, P} = build_scores(H, Target, T, GScore, FScore,
                                              Previous, Nodes),
            find_path(N, Target, G, F, P, Nodes)
    end.

build_scores(Current, Target ,OpenSet, GScore, FScore, Previous, Nodes) ->
    ScoreFun = fun(X) -> {X, maps:get(Current, GScore) + 1} end,
    FilterFun = fun({X, S}) -> S < maps:get(GScore, X) end,
    Scores = {Current, Target, OpenSet, GScore, FScore, Previous},

    Neighbours = get_neighbours(Current, Nodes),
    TentativeG = lists:map(ScoreFun, Neighbours),
    Better = lists:filter(FilterFun, TentativeG),
    lists:foldl(fun update_scores/2, Scores, Better).

get_neighbours({X, Y}, Nodes) ->
    Exists = fun(K) -> maps:is_key(K, Nodes) end,
    lists:filter(Exists, [{X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}]).

update_scores(S={Pos, Score},
              {Current, Target, OpenSet, GScore, FScore, Previous}) ->
    NewOpenSet = case lists:keymember(Pos, 1, OpenSet) of
                     true -> OpenSet;
                     false -> lists:keysort(2, [S|OpenSet])
                 end,
    {Current, NewOpenSet,
     GScore#{Pos => Score},
     FScore#{Pos => Score + min_path_cost(Pos, Target)},
     Previous#{Pos => Current}}.

is_goal(P0, P1) ->
    min_path_cost(P0, P1) =< 1.

build_path(Previous, Current) ->
    case maps:get(Current, Previous, undefined) of
        undefined -> [];
        V -> [Current|build_path(Previous, V)]
    end.

to_instructions([]) -> [];
to_instructions([_]) -> [];
to_instructions([H,N|T]) ->
    [pos_to_move(H, N)|to_instructions(T)].

pos_to_move({X0, _}, {X1, _}) when X0 < X1 -> 4;
pos_to_move({X0, _}, {X1, _}) when X0 > X1 -> 3;
pos_to_move({_, Y0}, {_, Y1}) when Y0 < Y1 -> 1;
pos_to_move(_, _) -> 2.
