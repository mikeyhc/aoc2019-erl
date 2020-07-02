-module(fft).

-export([decode/2]).

-compile(export_all).

decode(StrInput, Phases) ->
    Input = lists:map(fun(X) -> X - $0 end, StrInput),
    V = lists:foldl(fun(_, I) -> run_phase(I) end, Input, lists:seq(1, Phases)),
    lists:map(fun(X) -> X + $0 end, V).

run_phase(Input) ->
    Pattern = [0, 1, 0, -1],
    F = fun(N, I) ->
                P = build_patten(N, Pattern, length(Input)),
                [transform(Input, P)|I]
        end,
    Output = lists:foldl(F, [], lists:seq(1, length(Input))),
    lists:reverse(Output).

transform(Input, Pattern) ->
    Zipped = lists:zip(Input, Pattern),
    Multi = lists:map(fun({A, B}) -> A * B end, Zipped),
    abs(lists:sum(Multi) rem 10).

build_patten(N, Base, Length) ->
    P0 = lists:flatten(lists:map(fun(X) -> lists:duplicate(N, X) end, Base)),
    Ratio = Length / (length(P0) - 1),
    P1 = lists:flatten(lists:duplicate(ceil(Ratio), P0)),
    [_|P2] = lists:sublist(P1, 1, Length + 1),
    P2.
