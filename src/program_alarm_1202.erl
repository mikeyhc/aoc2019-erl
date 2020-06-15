-module(program_alarm_1202).

-export([instructions_from_file/1, process_instructions/1,
         determine_noun_verb/2]).

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

process_instructions(Instructions) ->
    process_instructions(Instructions, 0).

add(X, Y) -> X + Y.

multi(X, Y) -> X * Y.

instruction_op(Fun, Pos0, Pos1, Instructions) ->
    Fun(array:get(array:get(Pos0, Instructions), Instructions),
        array:get(array:get(Pos1, Instructions), Instructions)).

process_instructions(Instructions, IP) ->
    case array:get(IP, Instructions) of
        1 ->
            I = array:set(array:get(IP + 3, Instructions),
                          instruction_op(fun add/2, IP + 1, IP + 2,
                                         Instructions),
                          Instructions),
            process_instructions(I, IP + 4);
        2 ->
            I = array:set(array:get(IP + 3, Instructions),
                          instruction_op(fun multi/2, IP + 1, IP + 2,
                                         Instructions),
                          Instructions),
            process_instructions(I, IP + 4);
        99 -> Instructions;
        _ -> throw(invalid_instruction)
    end.

determine_noun_verb(Target, Instructions) ->
    Base = array:get(0, process_instructions(Instructions)),
    NounInstructions = array:set(1, 1, Instructions),
    NounDiff = array:get(0, process_instructions(NounInstructions)) - Base,
    VerbInstructions = array:set(2, 1, Instructions),
    VerbDiff = array:get(0, process_instructions(VerbInstructions)) - Base,
    Noun = (Target - Base) div NounDiff,
    Verb = (Target - Base - Noun * NounDiff) div VerbDiff,
    {Noun, Verb}.
