-module(ore).
-export([reactions_from_file/1, reaction_cost/1, fuel_from_ore/2]).

-record(reaction, {output :: string(),
                   qty :: pos_integer(),
                   tier :: non_neg_integer(),
                   source :: {string(), pos_integer()}
                  }).

%% read file

reactions_from_file(Filename) ->
    Lines = read_lines(Filename),
    Parts = lists:map(fun(X) -> string:split(X, " => ") end, Lines),
    Split = lists:map(fun([In,Out]) -> {string:split(In, ", ", all),Out} end,
                      Parts),
    Reacts = lists:map(fun({In, Out}) -> build_reaction(In, Out) end, Split),
    Map = lists:foldl(fun(X, Acc) -> Acc#{X#reaction.output => X} end, #{},
                      Reacts),
    apply_tiers(Map).

read_lines(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    String = binary:bin_to_list(Bin),
    lists:filter(fun(X) -> X =/= [] end, string:split(String, "\n", all)).

build_reaction(In, Out) ->
    InTuples = lists:map(fun qty_name_to_tuple/1, In),
    {Name, Qty} = qty_name_to_tuple(Out),
    #reaction{output=Name, qty=Qty, tier=0, source=InTuples}.

qty_name_to_tuple(String) ->
    [Qty,Name] = string:split(String, " ", all),
    {Name, list_to_integer(Qty)}.

apply_tiers(Reactions) ->
    apply_tiers([{"FUEL", 0}], Reactions).

apply_tiers([], Reactions) -> Reactions;
apply_tiers([{"ORE", _}|T], Reactions) ->
    apply_tiers(T, Reactions);
apply_tiers([{Element, Tier}|T], Reactions) ->
    Reaction = maps:get(Element, Reactions),
    Source = Reaction#reaction.source,
    TierSource = lists:map(fun({X, _}) -> {X, Tier+1} end, Source),
    NewReactions = Reactions#{Element => Reaction#reaction{tier = Tier}},
    apply_tiers(T ++ TierSource, NewReactions).

%% part 1

reaction_cost(Reactions) ->
    reaction_cost(Reactions, 1).

reaction_cost(Reactions, Amount) ->
    Tiered = tier_list(Reactions),
    Cost = reaction_cost(Tiered, Reactions, #{"FUEL" => {Amount, []}}),
    {Value, _} = maps:get("ORE", Cost),
    Value.

tier_list(Reactions) ->
    List = maps:to_list(Reactions),
    Values = lists:map(fun({_, V}) -> V end, List),
    lists:keysort(#reaction.tier, Values).

reaction_cost([], _Reactions, Costs) -> Costs;
reaction_cost([Reaction|T], Reactions, Costs) ->
    Output = Reaction#reaction.output,
    {Required, _} = maps:get(Output, Costs),
    Actual = ceil(Required / Reaction#reaction.qty),
    Source = Reaction#reaction.source,
    F = fun(E, C) -> update_cost(Output, E, Actual, C) end,
    NewCosts = lists:foldl(F, Costs, Source),
    reaction_cost(T, Reactions, NewCosts).

update_cost(Output, {Element, Price}, Runs, Cost) ->
    {Current, Requires} = maps:get(Element, Cost, {0, []}),
    Required = Price * Runs,
    NewRequires = [{Output, Required}|Requires],
    maps:put(Element, {Required + Current, NewRequires}, Cost).

%% part 2

fuel_from_ore(Ore, Reactions) ->
    fuel_from_ore(0, Ore, Ore, Reactions).

fuel_from_ore(Start, End, _Max, _Reactions) when Start == End - 1 -> Start;
fuel_from_ore(Start, End, Max, Reactions) ->
    Mid = (End - Start) div 2 + Start,
    Ore = reaction_cost(Reactions, Mid),
    if Ore > Max -> fuel_from_ore(Start, Mid, Max, Reactions);
       true -> fuel_from_ore(Mid, End, Max, Reactions)
    end.
