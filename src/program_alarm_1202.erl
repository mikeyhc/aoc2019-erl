-module(program_alarm_1202).

-export([determine_noun_verb/2]).

determine_noun_verb(Target, Intcode0) ->
    {ok, BaseIntcode} = intcode:run_intcode(Intcode0),
    Base = intcode:get_instruction(0, BaseIntcode),

    NounIntcode0 = intcode:set_instruction(1, 1, Intcode0),
    {ok, NounIntcode1} = intcode:run_intcode(NounIntcode0),
    NounDiff = intcode:get_instruction(0, NounIntcode1) - Base,

    VerbIntcode0 = intcode:set_instruction(2, 1, Intcode0),
    {ok, VerbIntcode1} = intcode:run_intcode(VerbIntcode0),
    VerbDiff = intcode:get_instruction(0, VerbIntcode1) - Base,

    Noun = (Target - Base) div NounDiff,
    Verb = (Target - Base - Noun * NounDiff) div VerbDiff,
    {Noun, Verb}.
