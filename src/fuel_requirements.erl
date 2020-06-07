-module(fuel_requirements).

-export([fuel_sum_from_file/1, fuel_cost/1, additional_fuel/1,
         total_fuel_sum_from_file/1]).

fuel_cost(X) -> X div 3 - 2.

fuel_sum_from_file(Filename) ->
    Lines = read_lines(Filename),
    Values = lists:map(fun list_to_integer/1, Lines),
    lists:foldl(fun(X, Acc) -> Acc + fuel_cost(X) end, 0, Values).

total_fuel_sum_from_file(Filename) ->
    Lines = read_lines(Filename),
    Values = lists:map(fun list_to_integer/1, Lines),
    F = fun(X, Acc) -> Acc + additional_fuel(fuel_cost(X)) end,
    lists:foldl(F, 0, Values).

additional_fuel(Fuel) ->
    additional_fuel(Fuel, Fuel).

additional_fuel(Fuel, Total) ->
    ExtraFuel = fuel_cost(Fuel),
    if ExtraFuel =< 0 -> Total;
       true -> additional_fuel(ExtraFuel, Total + ExtraFuel)
    end.

read_lines(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    List = binary:bin_to_list(Bin),
    Lines = string:split(List, "\n", all),
    lists:filter(fun(X) -> X =/= "" end, Lines).
