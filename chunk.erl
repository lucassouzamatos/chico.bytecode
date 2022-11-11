-module(chunk).
-export([disassemble/1, test/0, start_chunk/0, debug_value/1, get_instruction/2]).

-include("schema.hrl").

debug_value({value, Value}) -> io:format("value: ~w ~n", [Value]).

debug_instruction(Op) -> io:format("instruction: ~w ~n", [Op]).

start_chunk() ->
  #chunk{instructions = []}.

get_instruction(#chunk{instructions = Instructions} = _Chunk, Offset) ->
  lists:nth(Offset, Instructions).

disassemble_instruction(Chunk, Offset)  -> 
    Instruction = get_instruction(Chunk, Offset),
    debug_instruction(Instruction),
    case Instruction of
      {op, return} -> Offset + 1;
      {op, constant} -> 
        Value = get_instruction(Chunk, Offset + 1),
        debug_value(Value),
        Offset + 2
      end.

disassemble(Chunk) -> disassemble(Chunk, 1).
disassemble(#chunk{ instructions = Instructions } = Chunk, Offset) when Offset =< length(Instructions) ->
  NewOffset = disassemble_instruction(Chunk, Offset),
  disassemble(Chunk, NewOffset);
disassemble(_Chunk, Offset) ->
  Offset.

test() -> 
  Chunk = start_chunk(),
  Chunk1 = Chunk#chunk{instructions = [{op, constant}, {value, 1}]},
  disassemble(Chunk1).

