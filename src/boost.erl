-module(boost).

-export([boost_test/1, distress_code/1]).

boost_test(Intcode0) ->
    Intcode1 = intcode:set_input([1], Intcode0),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {[Output], _} = intcode:pop_all_output(Intcode2),
    Output.

distress_code(Intcode0) ->
    Intcode1 = intcode:set_input([2], Intcode0),
    {ok, Intcode2} = intcode:run_intcode(Intcode1),
    {[Output], _} = intcode:pop_all_output(Intcode2),
    Output.
