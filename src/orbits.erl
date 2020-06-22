-module(orbits).

-export([orbits_from_file/1, orbit_count_checksum/1, transfer_path/3]).

-record(orbit_map, {orbits=#{} :: maps:map(),
                    orbited=#{} :: maps:map()}).

build_orbits([], Map) -> Map;
build_orbits([H|T], #orbit_map{orbits=Orbits, orbited=Orbited}) ->
    [Center, Orbit] = string:split(H, ")"),
    Orbits0 = Orbits#{Orbit => Center},
    Children = maps:get(Center, Orbited, []),
    Orbited0 = Orbited#{Center => [Orbit|Children]},
    build_orbits(T, #orbit_map{orbits=Orbits0, orbited=Orbited0}).

orbits_from_file(Filename) ->
    Lines = read_lines(Filename),
    build_orbits(Lines, #orbit_map{}).

read_lines(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    List = binary:bin_to_list(Bin),
    Lines = string:split(List, "\n", all),
    lists:filter(fun(X) -> X =/= "" end, Lines).

orbit_count_checksum(#orbit_map{orbited=Orbited}) ->
    orbit_count_checksum([{"COM", 0}], 0, Orbited).

orbit_count_checksum([], Sum, _Orbtied) -> Sum;
orbit_count_checksum([{Name, Cost}|T], Sum, Orbited) ->
    Children = maps:get(Name, Orbited, []),
    Costed = lists:map(fun(X) -> {X, Cost+1} end, Children),
    orbit_count_checksum(Costed ++ T, Sum + Cost, Orbited).

transfer_path(Source, Target, #orbit_map{orbits=Orbits}) ->
    Path = path_to_com(Source, -1, #{}, Orbits),
    {Path, to_path_intersection(Target, -1, Path, Orbits)}.

path_to_com("COM", Cost, Costs, _Orbits) -> Costs#{"COM" => Cost};
path_to_com(Source, Cost, Costs, Orbits) ->
    #{Source := Next} = Orbits,
    path_to_com(Next, Cost+1, Costs#{Source => Cost}, Orbits).

to_path_intersection(Source, Cost, Path, Orbits) ->
    case maps:is_key(Source, Path) of
        true -> Cost + maps:get(Source, Path);
        false ->
            #{Source := Next} = Orbits,
            to_path_intersection(Next, Cost+1, Path, Orbits)
    end.

