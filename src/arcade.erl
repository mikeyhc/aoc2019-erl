-module(arcade).

-export([count_tiles/1, run_game/1]).

count_tiles(Intcode0) ->
    {ok, Intcode1} = intcode:run_intcode(Intcode0),
    {Output, _} = intcode:pop_all_output(Intcode1),
    {Render, _Score} = build_render(Output),
    Tiles = lists:map(fun({_, V}) when V =:= 2 -> 1;
                         (_) -> 0
                      end,
                      maps:to_list(Render)),
    lists:sum(Tiles).

build_render(Output) ->
    build_render(Output, #{}, 0).

build_render([], Output, Score) -> {Output, Score};
build_render([-1,0,V|T], Output, _Score) ->
    build_render(T, Output, V);
build_render([X,Y,I|T], Output, Score) ->
    build_render(T, Output#{{X, Y} => I}, Score).

run_game(Intcode0) ->
    Intcode1 = intcode:set_instruction(0, 2, Intcode0),
    Status = intcode:intcode_status(Intcode1),
    run(Status, Intcode1, #{}, 0).

run(halted, _Intcode, Output, Score) ->
    render(Output, Score);
run(_Status, Intcode0, OldOutput, Score) ->
    {ok, Intcode1} = intcode:run_intcode(Intcode0),
    {Output, Intcode2} = intcode:pop_all_output(Intcode1),
    {NewOutput, NewScore} = update_render(OldOutput, Output, Score),
    {BallX, _} = get_ball(NewOutput),
    {PaddleX, _} = get_paddle(NewOutput),
    Instruction = paddle_instruction(PaddleX, BallX),
    Intcode3 = intcode:add_input([Instruction], Intcode2),
    run(intcode:intcode_status(Intcode3), Intcode3, NewOutput, NewScore).

render(Render, Score) ->
    Rows = get_rows(maps:to_list(Render)),
    render_rows(Rows),
    io:format("score: ~p~n", [Score]).

get_rows(L) ->
    F = fun({{X, Y}, T}, A) -> A#{Y => [{X,T}|maps:get(Y, A, [])]} end,
    M = lists:foldl(F, #{}, L),
    lists:sort(fun({K0, _}, {K1, _}) -> K0 < K1 end, maps:to_list(M)).

render_rows(L) -> render_rows(L, 0).

render_rows([], _) -> io:format("~n");
render_rows([{R, _}|T], N) when R < N ->
    io:format("~n"),
    render_rows(T, N+1);
render_rows([{_, L}|T], N) ->
    render_row(L),
    render_rows(T, N+1).

render_row(L) ->
    Sorted = lists:sort(fun({X0, _}, {X1, _}) -> X0 < X1 end, L),
    render_row(Sorted, 0).

render_row([], _) -> io:format("~n");
render_row([{X, _}|T], N) when N < X ->
    io:format(" "),
    render_row(T, N+1);
render_row([{_, I}|T], N) ->
    io:format("~s", [[get_char(I)]]),
    render_row(T, N+1).

get_char(0) -> $ ;
get_char(1) -> $#;
get_char(2) -> $@;
get_char(3) -> $_;
get_char(4) -> $o.

get_ball(M) when is_map(M) -> get_ball(maps:to_list(M));
get_ball([{P, 4}|_]) -> P;
get_ball([_|T]) -> get_ball(T).

get_paddle(M) when is_map(M) -> get_paddle(maps:to_list(M));
get_paddle([{P, 3}|_]) -> P;
get_paddle([_|T]) -> get_paddle(T).

paddle_instruction(P, B) when P > B -> -1;
paddle_instruction(P, B) when P < B -> 1;
paddle_instruction(_, _) -> 0.

update_render(OldRender, NewRender, Score) ->
    build_render(NewRender, OldRender, Score).
