-module(moons).

-export([moons_from_file/1, run_moons/1, run_moons/2, energy/1,
         predict_moon_cycle/1]).

moons_from_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    String = binary:bin_to_list(Bin),
    Parts = lists:filter(fun(X) -> X =/= [] end,
                         string:split(String, "\n", all)),
    lists:map(fun str_to_moon/1, Parts).

str_to_moon("<" ++ Rest) ->
    {Coords, _} = lists:split(length(Rest) - 1, Rest),
    Parts = string:split(Coords, ", ", all),
    [X, Y, Z] = lists:map(fun([_,$=|Num]) -> list_to_integer(Num) end, Parts),
    {{X, Y, Z}, {0, 0, 0}}.

run_moons(Moons, Steps) ->
    lists:foldl(fun(_, M) -> run_moons(M) end, Moons, lists:seq(1, Steps)).

run_moons(Moons) ->
    {Positions, Velocities} = lists:unzip(Moons),
    NewVelocities = update_velocities(Positions, Velocities),
    NewPositions = update_positions(Positions, NewVelocities),
    lists:zip(NewPositions, NewVelocities).

tuple_list_element(N, List) ->
    lists:map(fun(T) -> element(N, T) end, List).

update_single_velocity(Num, Positions, Velocities) ->
    Ps = tuple_list_element(Num, Positions),
    Vs = tuple_list_element(Num, Velocities),
    Diff = calculate_velocity_difference(Ps),
    lists:zipwith(fun(D, V) -> D + V end, Diff, Vs).

update_velocities(Positions, Velocities) ->
    NewX  = update_single_velocity(1, Positions, Velocities),
    NewY  = update_single_velocity(2, Positions, Velocities),
    NewZ  = update_single_velocity(3, Positions, Velocities),
    lists:zip3(NewX, NewY, NewZ).

calculate_velocity_difference(P) ->
    calculate_velocity_difference(P, P, []).

calculate_velocity_difference([], _Moons, Acc) -> lists:reverse(Acc);
calculate_velocity_difference([H|T], Moons, Acc) ->
    N = deltaV(H, Moons),
    calculate_velocity_difference(T, Moons, [N|Acc]).

update_single_position(Num, Positions, Velocities) ->
    Ps = tuple_list_element(Num, Positions),
    Vs = tuple_list_element(Num, Velocities),
    lists:zipwith(fun(P, V) -> P + V end, Ps, Vs).

update_positions(Positions, Velocities) ->
    NewX = update_single_position(1, Positions, Velocities),
    NewY = update_single_position(2, Positions, Velocities),
    NewZ = update_single_position(3, Positions, Velocities),
    lists:zip3(NewX, NewY, NewZ).

moon_energy({{X, Y, Z}, {A, B, C}}) ->
    (abs(X) + abs(Y) + abs(Z)) * (abs(A) + abs(B) + abs(C)).

energy(Moons) ->
    lists:foldl(fun(X, Acc) -> moon_energy(X) + Acc end, 0, Moons).

make_pos_vel(N, L) ->
    lists:map(fun(X) -> {element(N, X), 0} end, L).

predict_moon_cycle(Moons) ->
    % plug the output into a LCM calculator
    {_, {_, [X|_]}} = find_cycles(1, Moons),
    {_, {_, [Y|_]}} = find_cycles(2, Moons),
    {_, {_, [Z|_]}} = find_cycles(3, Moons),
    {X, Y, Z}.

find_cycles(Pos, Moons) ->
    Xs = make_pos_vel(Pos, Moons),
    XPos = run_pos(Xs, 1000000),
    Encounters = encounters(XPos),
    Deltas = deltas(Encounters),
    F = fun({_, {_, L}}) -> L =/= [] end,
    [H|_] = lists:filter(F, maps:to_list(Deltas)),
    H.

step({Pos, Vel}, Moons) ->
    V =  Vel + deltaV(Pos, Moons),
    {Pos + V, V}.

deltaV(Pos, Moons) ->
    F = fun({X, _}, A) when Pos < X -> A + 1;
           ({X, _}, A) when Pos > X -> A - 1;
           (_, A) -> A
        end,
    lists:foldl(F, 0, Moons).

run_pos(Moons, N) ->
    run_pos(Moons, N, []).

run_pos(_Moons, 0, Positions) -> lists:reverse(Positions);
run_pos(Moons, N, Positions) ->
    NewMoons = lists:map(fun(M) -> step(M, Moons) end, Moons),
    run_pos(NewMoons, N - 1, [Moons|Positions]).

encounters(Positions) ->
    encounters(Positions, 1, #{}).

encounters([], _, Map) -> Map;
encounters([H|T], N, Map) ->
    E =  maps:get(H, Map, []),
    encounters(T, N+1, Map#{H => [N|E]}).

deltas(Positions) ->
    F = fun(_, V) ->
                Fst = hd(lists:reverse(V)),
                Snd = subtract_pairs(lists:reverse(V)),
                {Fst, Snd}
        end,
    maps:map(F, Positions).

subtract_pairs([F,S|T]) -> [S-F|subtract_pairs(T)];
subtract_pairs(_) -> [].
