-module(wires).

-export([wires_from_file/1, closest_intersection/2]).

-record(wire_part, {p1 :: {integer(), integer()},
                    p2 :: {integer(), integer()},
                    distance :: integer()
                   }).

-record(wire, {vertical_parts :: [#wire_part{}],
               horizontal_parts :: [#wire_part{}]
              }).

walk_tokens([], _Last, _Dist, Vert, Hori) ->
    {lists:reverse(Vert), lists:reverse(Hori)};
walk_tokens([H|T], Last={X, Y}, Dist, Vert, Hori) ->
    [Dir|LenStr] = H,
    Len = list_to_integer(LenStr),
    NewDist = Dist + Len,
    case Dir of
        $\U ->
            Point = {X, Y+Len},
            Part = #wire_part{p1=Last, p2=Point, distance=Dist},
            walk_tokens(T, Point, NewDist, [Part|Vert], Hori);
        $\D ->
            Point = {X, Y-Len},
            Part = #wire_part{p1=Last, p2=Point, distance=Dist},
            walk_tokens(T, Point, NewDist, [Part|Vert], Hori);
        $\R ->
            Point = {X+Len, Y},
            Part = #wire_part{p1=Last, p2=Point, distance=Dist},
            walk_tokens(T, Point, NewDist, Vert, [Part|Hori]);
        $\L ->
            Point = {X-Len, Y},
            Part = #wire_part{p1=Last, p2=Point, distance=Dist},
            walk_tokens(T, Point, NewDist, Vert, [Part|Hori])
    end.

parse_wire(InputStr) ->
    Tokens = string:split(InputStr, ",", all),
    {Vert, Hori} = walk_tokens(Tokens, {0, 0}, 0, [], []),
    #wire{vertical_parts=Vert, horizontal_parts=Hori}.

wires_from_file(Filename) ->
    [Wire1Str, Wire2Str] = read_lines(Filename),
    Wire1 = parse_wire(Wire1Str),
    Wire2 = parse_wire(Wire2Str),
    {Wire1, Wire2}.

read_lines(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    List = binary:bin_to_list(Bin),
    Lines = string:split(List, "\n", all),
    lists:filter(fun(X) -> X =/= "" end, Lines).

closest_intersection(W1, W2) ->
    #wire{vertical_parts=V1, horizontal_parts=H1} = W1,
    #wire{vertical_parts=V2, horizontal_parts=H2} = W2,
    {Dist1, Walk1} = closest_intersection_distance(V1, H2),
    {Dist2, Walk2} = closest_intersection_distance(V2, H1),
    MinDist = if Dist1 < Dist2 -> Dist1;
                 true -> Dist2
              end,
    MinWalk = if Walk1 < Walk2 -> Walk1;
                 true -> Walk2
              end,
    {MinDist, MinWalk}.

closest_intersection_distance(V, H) ->
    F = fun(X, {Dist, Walk}) ->
                {D, W} = closest_intersection_distance_(X, H),
                NewDist = if Dist =:= undefined -> D;
                             D =:= undefined -> Dist;
                             D < Dist -> D;
                             true -> Dist
                          end,
                NewWalk = if Walk =:= undefined -> W;
                             W =:= undefined -> Walk;
                             W < Walk -> W;
                             true -> Walk
                          end,
                {NewDist, NewWalk}
        end,
    lists:foldl(F, {undefined, undefined}, V).

closest_intersection_distance_(Target, Lines) ->
    F = fun(L, {Dist, Walk}) ->
                NewDist = case intersect(Target, L) of
                              undefined -> Dist;
                              {X, Y} ->
                                  Sum = abs(X) + abs(Y),
                                  if Dist =:= undefined -> Sum;
                                     Dist < Sum -> Dist;
                                     true -> Sum
                                  end
                          end,
                NewWalk = case walk_intersect(Target, L) of
                              undefined -> Walk;
                              W ->
                                  if Walk =:= undefined -> W;
                                     Walk < W -> Walk;
                                     true -> W
                                  end
                          end,
                {NewDist, NewWalk}
        end,
    lists:foldl(F, {undefined, undefined}, Lines).

intersect(#wire_part{p1={X, Y1}, p2={X, Y2}},
          #wire_part{p1={X1, Y}, p2={X2, Y}}) ->
    {X1_, X2_} = if X1 < X2 -> {X1, X2};
                    true -> {X2, X1}
                 end,
    {Y1_, Y2_} = if Y1 < Y2 -> {Y1, Y2};
                    true -> {Y2, Y1}
                 end,
    if X >= X1_ andalso X =< X2_ andalso Y >= Y1_ andalso Y =< Y2_ -> {X, Y};
       true -> undefined
    end.

walk_intersect(#wire_part{p1={X, Y1}, p2={X, Y2}, distance=D1},
               #wire_part{p1={X1, Y}, p2={X2, Y}, distance=D2}) ->
    {X1_, X2_} = if X1 < X2 -> {X1, X2};
                    true -> {X2, X1}
                 end,
    {Y1_, Y2_} = if Y1 < Y2 -> {Y1, Y2};
                    true -> {Y2, Y1}
                 end,
    if X >= X1_ andalso X =< X2_ andalso Y >= Y1_ andalso Y =< Y2_ ->
           D1 + D2 + abs(Y1 - Y) + abs(X1 - X);
       true -> undefined
    end.
