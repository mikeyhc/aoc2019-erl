-module(fft).

-export([decode/2, decode/3]).

-compile(export_all).

input_from_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    string:trim(binary:bin_to_list(Bin)).

decode(StrInput, Phases) ->
    Input = lists:map(fun(X) -> X - $0 end, StrInput),
    F = fun(_, I) -> run_phase(I, 1) end,
    V = lists:foldl(F, Input, lists:seq(1, Phases)),
    lists:map(fun(X) -> X + $0 end, V).

decode(StrInput, Phases, Repeats) ->
    Input = lists:map(fun(X) -> X - $0 end, StrInput),
    OffsetFun = fun(X, Acc) -> Acc * 10 + X end,
    Offset = lists:foldl(OffsetFun, 0, lists:sublist(Input, 1, 7)),
    FullInput = lists:concat(lists:duplicate(Repeats, Input)),
    Suffix = drop(FullInput, Offset),
    Output = fast_transform(Suffix, Phases),
    lists:map(fun(X) -> X + $0 end, lists:sublist(Output, 1, 8)).

drop(L, 0) -> L;
drop([_|T], N) -> drop(T, N - 1).

run_phase(Input, Repeats) ->
    F = fun(N, Prev) -> [transform(N, Repeats, Input)|Prev] end,
    Output = lists:foldl(F, [], lists:seq(1, length(Input))),
    lists:reverse(Output).

transform(N, Repeats, I0) ->
    Length = length(I0),
    Cycle = lcm(Length, 4 * N),
    Count = (Length * Repeats) div Cycle,
    Remainder = (Length * Repeats) rem Cycle,
    % io:format("~p|~p|~p: ~p/~p~n", [Length, N, Cycle, Count, Remainder]),
    R0 = if Count > 0 -> transform_(I0, I0, Cycle, N, N - 1, drop, 0) * Count;
            true -> 0
         end,
    R0 + transform_(I0, I0, Remainder, N, N - 1, drop, 0).
    % transform_(I0, N, N - 1, drop, 0).

transform_(_, _, 0, _, _, _, Sum) ->  abs(Sum rem 10);
transform_([], Full, C, N, I, Phase, Sum) ->
    transform_(Full, Full, C, N, I, Phase, Sum);
transform_(L, Full, C, N, 0, Phase, Sum) ->
    transform_(L, Full, C, N, N, next_phase(Phase), Sum);
transform_([_|T], Full, C, N, I, drop, Sum) ->
    transform_(T, Full, C - 1, N, I - 1, drop, Sum);
transform_([_|T], Full, C, N, I, middrop, Sum) ->
    transform_(T, Full, C - 1, N, I - 1, middrop, Sum);
transform_([H|T], Full, C, N, I, pos, Sum) ->
    transform_(T, Full, C - 1, N, I - 1, pos, Sum + H);
transform_([H|T], Full, C, N, I, neg, Sum) ->
    transform_(T, Full, C - 1, N, I - 1, neg, Sum - H).

next_phase(drop) -> pos;
next_phase(pos) -> middrop;
next_phase(middrop) -> neg;
next_phase(neg) -> drop.

lcm(A, B) -> A * B div gcd(A, B).

gcd(A, 0) -> A;
gcd(A, B) ->
    gcd(B, A rem B).

scanr(F, Acc, List) ->
    lists:foldr(fun(X, L=[Last|_]) -> [F(X, Last)|L] end, [Acc], List).

fast_transform(Input, 0) -> Input;
fast_transform(Input, N) ->
    V = scanr(fun(D, S) -> (D + S) rem 10 end, 0, Input),
    fast_transform(V, N - 1).
