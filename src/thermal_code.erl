-module(thermal_code).

-export([thermal_code/1, radiators_code/1]).

thermal_code(Intcode0) ->
    Intcode1 = intcode:set_input([1], Intcode0),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {Output, _} = intcode:pop_all_output(Intcode2),
    Output.

radiators_code(Intcode0) ->
    Intcode1 = intcode:set_input([5], Intcode0),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {[Output], _} = intcode:pop_all_output(Intcode2),
    Output.
