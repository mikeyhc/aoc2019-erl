-module(asteroids).
-export([asteroids_from_file/1, asteroids_from_list/1, best_station/1,
         destroy_bet/3]).

asteroids_from_file(Filename) ->
    asteroids_from_list(read_file(Filename)).

asteroids_from_list(List) ->
    IdxLines = lists:zip(lists:seq(0, length(List)-1), List),
    lists:foldl(fun line_to_asteroids/2, sets:new(), IdxLines).

line_to_asteroids({Y, Line}, Asteroids) ->
    IdxChars = lists:zip(lists:seq(0, length(Line)-1), Line),
    Hits = lists:filter(fun({_, A}) -> A =:= $# end, IdxChars),
    F = fun({X, _}, A0) -> sets:add_element({X, Y}, A0) end,
    lists:foldl(F, Asteroids, Hits).

read_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    String = binary:bin_to_list(Bin),
    Lines = string:split(String, "\n", all),
    lists:filter(fun(X) -> X =/= "" end, Lines).

quadrant(V) when V < 0 -> neg;
quadrant(_) -> pos.

angle({X0, Y0}, {X, Y}) ->
    DX = X - X0,
    DY = Y - Y0,
    if DX =:= 0 -> {-100, quadrant(DX), quadrant(DY)};
       true -> {DY / DX, quadrant(DX), quadrant(DY)}
    end.

visible(P0, Possible) ->
    F = fun(P, S0) -> sets:add_element(angle(P0, P), S0) end,
    S0 = sets:fold(F, sets:new(), sets:del_element(P0, Possible)),
    sets:size(S0).

best_station(Asteroids) ->
    F0 = fun(X, Acc) -> [{X, visible(X, Asteroids)}|Acc] end,
    [H|_] = lists:reverse(lists:keysort(2, sets:fold(F0, [], Asteroids))),
    H.

distance({X0, Y0}, {X, Y}) ->
    math:sqrt(math:pow(X - X0, 2) + math:pow(Y - Y0, 2)).

clock_order({_, _, {D0, pos, neg}}, {_, _, {D1, pos, neg}}) -> D0 < D1;
clock_order({_, _, {_, pos, neg}}, _) -> true;
clock_order(_, {_, _, {_, pos, neg}}) -> false;

clock_order({_, _, {D0, pos, pos}}, {_, _, {D1, pos, pos}}) -> D0 < D1;
clock_order({_, _, {_, pos, pos}}, _) -> true;
clock_order(_, {_, _, {_, pos, pos}}) -> false;

clock_order({_, _, {D0, neg, pos}}, {_, _, {D1, neg, pos}}) -> D0 < D1;
clock_order({_, _, {_, neg, pos}}, _) -> true;
clock_order(_, {_, _, {_, neg, pos}}) -> false;

clock_order({_, _, {D0, neg, neg}}, {_, _, {D1, neg, neg}}) -> D0 < D1.

add_property(Key, Value, List) ->
    add_property(Key, Value, List, []).

add_property(Key, Value, [], Acc) -> lists:reverse([{Key, [Value]}|Acc]);
add_property(Key, Value, [{K,V}|Rest], Acc) when Key =:= K ->
    lists:reverse(Acc) ++ [{K,V ++ [Value]}|Rest];
add_property(Key, Value, [H|T], Acc) ->
    add_property(Key ,Value, T, [H|Acc]).

group_by_angle(List) ->
    F = fun(P={_, _, A}, D) -> add_property(A, P, D) end,
    lists:map(fun({_, X}) -> X end, lists:foldl(F, [], List)).

unwrap(L) -> unwrap(L, []).

unwrap([], Acc) -> lists:reverse(Acc);
unwrap([[H|T]|Rest], Acc) ->
    unwrap(Rest ++ [T], [H|Acc]);
unwrap([[]|Rest], Acc) -> unwrap(Rest, Acc).

distances(P0, Possible) ->
    F = fun(P) -> {P, distance(P0, P), angle(P0, P)} end,
    lists:map(F, sets:to_list(sets:del_element(P0, Possible))).

destroy_bet(Point, Bet, Asteroids) ->
    L0 = distances(Point, Asteroids),
    L1 = lists:keysort(2, L0),
    L2 = lists:sort(fun clock_order/2, L1),
    lists:nth(Bet, unwrap(group_by_angle(L2))).
