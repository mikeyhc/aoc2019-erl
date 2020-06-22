-module(intcode).
-export([intcode_from_file/1, run_intcode/1, set_input/2, add_input/2,
         pop_all_output/1, get_instruction/2, set_instruction/3]).

-type intcode_status() :: ready | blocked | halted.

-record(intcode_pipe, {buffer :: [integer()]}).
-type pipe() :: #intcode_pipe{}.

-record(intcode, {instructions :: [integer()],
                  pointer :: non_neg_integer(),
                  status :: intcode_status(),
                  input :: pipe(),
                  output :: pipe()
                 }).
-type intcode() :: #intcode{}.

-spec intcode_from_file(file:name_all()) -> intcode().
intcode_from_file(Filename) ->
    Tokens = read_parts(Filename),
    IntCodes = lists:map(fun list_to_integer/1, Tokens),
    #intcode{instructions=array:from_list(IntCodes, 0),
             pointer=0,
             status=ready,
             input=[],
             output=[]}.

read_parts(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    String = binary:bin_to_list(Bin),
    List = string:trim(String),
    Lines = string:split(List, ",", all),
    lists:filter(fun(X) -> X =/= "" end, Lines).

-spec add_input(integer() | [integer()], intcode()) -> intcode().
add_input(NewInput, I=#intcode{input=Input}) when is_list(NewInput) ->
    I#intcode{input=Input ++ NewInput};
add_input(NewInput, I=#intcode{input=Input}) ->
    I#intcode{input=Input ++ [NewInput]}.

-spec set_input([integer()], intcode()) -> intcode().
set_input(Input, Intcode) ->
    Intcode#intcode{input=Input}.

-spec run_intcode(intcode()) -> {ok, intcode()} | {status, intcode_status()}.
run_intcode(Intcode=#intcode{status=ready}) ->
    #intcode{instructions=Instructions,
             input=Input,
             pointer=IP,
             output=Output} = Intcode,
    process_instructions(Input, [], Instructions, IP, Output);
run_intcode(#intcode{status=Status}) ->
    {status, Status}.

-spec pop_all_output(intcode()) -> {[integer()], intcode()}.
pop_all_output(I=#intcode{output=Output}) ->
    {Output, I#intcode{output=[]}}.

-spec get_instruction(non_neg_integer(), intcode()) -> integer().
get_instruction(Idx, #intcode{instructions=Instructions}) ->
    array:get(Idx, Instructions).

-spec set_instruction(non_neg_integer(), integer(), intcode()) -> intcode().
set_instruction(Idx, Val, I=#intcode{instructions=Instructions}) ->
    I#intcode{instructions=array:set(Idx, Val, Instructions)}.

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

process_instructions(Input, Output, Instructions, IP, OldOutput) ->
    Instruction = array:get(IP, Instructions),
    {Op1Mode, Op2Mode, _Op3Mode, Opcode} = parse_instruction(Instruction),
    io:format("~p: ~p ~p ~p ~p ~n", [IP, Instruction,
                                     array:get(IP+1, Instructions),
                                     array:get(IP+2, Instructions),
                                     array:get(IP+3, Instructions)
                                    ]),
    case Opcode of
        1 ->
            % add
            I = array:set(array:get(IP+3, Instructions),
                          instruction_op(fun add/2, IP+1, IP+2,
                                         Op1Mode, Op2Mode,
                                         Instructions),
                          Instructions),
            process_instructions(Input, Output, I, IP+4, OldOutput);
        2 ->
            % multiply
            I = array:set(array:get(IP+3, Instructions),
                          instruction_op(fun multi/2, IP+1, IP+2,
                                         Op1Mode, Op2Mode,
                                         Instructions),
                          Instructions),
            process_instructions(Input, Output, I, IP+4, OldOutput);
        3 ->
            % save input
            [H|T] = Input,
            I = array:set(array:get(IP+1, Instructions), H, Instructions),
            process_instructions(T, Output, I, IP+2, OldOutput);
        4 ->
            % write to output
            O = op_value(IP+1, Op1Mode, Instructions),
            process_instructions(Input, [O|Output], Instructions, IP+2,
                                 OldOutput);
        5 ->
            % jump-if-true
            Op1 = op_value(IP+1, Op1Mode, Instructions),
            Op2 = op_value(IP+2, Op2Mode, Instructions),
            if Op1 =:= 0 ->
                   process_instructions(Input, Output, Instructions, IP+3,
                                        OldOutput);
               true ->
                   process_instructions(Input, Output, Instructions, Op2,
                                        OldOutput)
            end;
        6 ->
            Op1 = op_value(IP+1, Op1Mode, Instructions),
            Op2 = op_value(IP+2, Op2Mode, Instructions),
            if Op1 =:= 0 ->
                   process_instructions(Input, Output, Instructions, Op2,
                                        OldOutput);
               true ->
                   process_instructions(Input, Output, Instructions, IP+3,
                                        OldOutput)
            end;
        7 ->
            % less than
            LT = instruction_op(fun lt/2, IP+1, IP+2, Op1Mode, Op2Mode,
                                Instructions),
            Op3 = array:get(IP+3, Instructions),
            I = if LT -> array:set(Op3, 1, Instructions);
                   true -> array:set(Op3, 0, Instructions)
                end,
            process_instructions(Input, Output, I, IP+4, OldOutput);
        8 ->
            % equals
            Eq = instruction_op(fun equals/2, IP+1, IP+2, Op1Mode, Op2Mode,
                                Instructions),
            Op3 = array:get(IP+3, Instructions),
            I = if Eq -> array:set(Op3, 1, Instructions);
                   true -> array:set(Op3, 0, Instructions)
                end,
            process_instructions(Input, Output, I, IP+4, OldOutput);
        99 ->
            {ok, #intcode{instructions=Instructions,
                          pointer=IP,
                          status=halted,
                          input=Input,
                          output=lists:reverse(Output) ++ OldOutput}};
        _ -> throw({invalid_instruction, Instruction, IP})
    end.
