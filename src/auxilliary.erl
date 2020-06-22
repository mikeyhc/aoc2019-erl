-module(auxilliary).

-export([compute_auxilliary/2, compute_max_auxilliary/3]).

run_auxilliary(Phase, I, Intcode0) ->
    io:format("testing phase ~p:~p~n", [Phase, I]),
    Intcode1 = intcode:set_input([Phase, I], Intcode0),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {[Output], _} = intcode:pop_all_output(Intcode2),
    Output.

compute_auxilliary(Phases, Intcode) ->
    lists:foldl(fun(P, I) -> run_auxilliary(P, I, Intcode) end, 0, Phases).

compute_max_auxilliary(Intcode, Start, End) ->
    F = fun(X) -> compute_auxilliary(X, Intcode) end,
    lists:max(lists:map(F, phase_range(Start, End))).

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

phase_range(Start, End) ->
    perms(lists:seq(Start, End)).
