-module(thermal_code).

-export([instructions_from_file/1, process_instructions/2]).

instructions_from_file(Filename) ->
    Tokens = read_parts(Filename),
    IntCodes = lists:map(fun list_to_integer/1, Tokens),
    array:from_list(IntCodes, 0).

read_parts(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    String = binary:bin_to_list(Bin),
    List = string:trim(String),
    Lines = string:split(List, ",", all),
    lists:filter(fun(X) -> X =/= "" end, Lines).

process_instructions(Input, Instructions) ->
    process_instructions(Input, [], Instructions, 0).

add(X, Y) -> X + Y.

multi(X, Y) -> X * Y.

lt(X, Y) -> X < Y.

equals(X, Y) -> X =:= Y.

op_value(Pos, OpMode, Instructions) ->
    Val = array:get(Pos, Instructions),
    case OpMode of
        0 -> array:get(Val, Instructions);
        1 -> Val;
        _ -> throw(invalid_op_mode)
    end.

instruction_op(Fun, Pos0, Pos1, Op1Mode, Op2Mode, Instructions) ->
    Fun(op_value(Pos0, Op1Mode, Instructions),
        op_value(Pos1, Op2Mode, Instructions)).

parse_instruction(Instruction) ->
    Opcode = Instruction rem 100,
    I0 = Instruction div 100,
    Op1Mode = I0 rem 10,
    I1 = I0 div 10,
    Op2Mode = I1 rem 10,
    I2 = I1 div 10,
    Op3Mode = I2 rem 10,
    {Op1Mode, Op2Mode, Op3Mode, Opcode}.

process_instructions(Input, Output, Instructions, IP) ->
    Instruction = array:get(IP, Instructions),
    {Op1Mode, Op2Mode, _Op3Mode, Opcode} = parse_instruction(Instruction),
    %io:format("~p: ~p ~p ~p ~p ~n", [IP, Instruction,
    %                                 array:get(IP+1, Instructions),
    %                                 array:get(IP+2, Instructions),
    %                                 array:get(IP+3, Instructions)
    %                                ]),
    case Opcode of
        1 ->
            % add
            I = array:set(array:get(IP+3, Instructions),
                          instruction_op(fun add/2, IP+1, IP+2,
                                         Op1Mode, Op2Mode,
                                         Instructions),
                          Instructions),
            process_instructions(Input, Output, I, IP+4);
        2 ->
            % multiply
            I = array:set(array:get(IP+3, Instructions),
                          instruction_op(fun multi/2, IP+1, IP+2,
                                         Op1Mode, Op2Mode,
                                         Instructions),
                          Instructions),
            process_instructions(Input, Output, I, IP+4);
        3 ->
            % save input
            [H|T] = Input,
            I = array:set(array:get(IP+1, Instructions), H, Instructions),
            process_instructions(T, Output, I, IP+2);
        4 ->
            % write to output
            O = op_value(IP+1, Op1Mode, Instructions),
            process_instructions(Input, [O|Output], Instructions, IP+2);
        5 ->
            % jump-if-true
            Op1 = op_value(IP+1, Op1Mode, Instructions),
            Op2 = op_value(IP+2, Op2Mode, Instructions),
            if Op1 =:= 0 ->
                   process_instructions(Input, Output, Instructions, IP+3);
               true ->
                   process_instructions(Input, Output, Instructions, Op2)
            end;
        6 ->
            Op1 = op_value(IP+1, Op1Mode, Instructions),
            Op2 = op_value(IP+2, Op2Mode, Instructions),
            if Op1 =:= 0 ->
                   process_instructions(Input, Output, Instructions, Op2);
               true ->
                   process_instructions(Input, Output, Instructions, IP+3)
            end;
        7 ->
            % less than
            LT = instruction_op(fun lt/2, IP+1, IP+2, Op1Mode, Op2Mode,
                                Instructions),
            Op3 = array:get(IP+3, Instructions),
            I = if LT -> array:set(Op3, 1, Instructions);
                   true -> array:set(Op3, 0, Instructions)
                end,
            process_instructions(Input, Output, I, IP+4);
        8 ->
            % equals
            Eq = instruction_op(fun equals/2, IP+1, IP+2, Op1Mode, Op2Mode,
                                Instructions),
            Op3 = array:get(IP+3, Instructions),
            I = if Eq -> array:set(Op3, 1, Instructions);
                   true -> array:set(Op3, 0, Instructions)
                end,
            process_instructions(Input, Output, I, IP+4);
        99 -> {lists:reverse(Output), Instructions};
        _ -> throw({invalid_instruction, Instruction, IP})
    end.
