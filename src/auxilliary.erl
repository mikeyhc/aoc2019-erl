-module(auxilliary).

-export([run_auxilliary/2, compute_max_auxilliary/3]).

run_auxilliary_(Input, [Intcode0|Machines]) ->
    Intcode1 = intcode:add_input(Input, Intcode0),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {Output, Intcode3} = intcode:pop_all_output(Intcode2),
    case Machines of
        [] ->
            [H|_] = Output,
            H;
        _ -> case intcode:intcode_status(Intcode2) of
                 halted -> run_auxilliary_(Output, Machines);
                 _ -> run_auxilliary_(Output, Machines ++ [Intcode3])
             end
    end.

run_auxilliary(Phase, Intcode) ->
    Machines = lists:map(fun(X) -> intcode:set_input([X], Intcode) end, Phase),
    run_auxilliary_([0], Machines).

compute_max_auxilliary(Intcode, Start, End) ->
    F = fun(X) -> run_auxilliary(X, Intcode) end,
    lists:max(lists:map(F, phase_range(Start, End))).

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

phase_range(Start, End) ->
    perms(lists:seq(Start, End)).
