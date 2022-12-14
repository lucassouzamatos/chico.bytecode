-module(vm).
-export([interpret/1, test/0]).

-include("schema.hrl").

push_stack([], Stack) -> Stack;
push_stack([H | T], Stack) -> 
  push_stack(T, [H | Stack]).

pop_stack([H|T])-> {H,T}.

update_vm_stack({push, Value}, #vm{stack = Stack} = Vm) ->
  Vm#vm{stack = push_stack([Value], Stack)};
update_vm_stack({pop}, #vm{stack = Stack} = Vm) ->
  {H, T} = pop_stack(Stack),
  {H, Vm#vm{stack = T}}.

run(Vm) ->
  run(Vm, 1).
run(#vm{chunk = Chunk} = Vm, Offset) ->
  #chunk{ instructions = Instructions } = Chunk,
  if length(Instructions) >= Offset ->
      Instruction = chunk:get_instruction(Chunk, Offset),
      
      case run_instruction(Instruction, Vm, Offset) of
        {ok, Value, UpdatedVm, UpdateOffset} -> 
          chunk:debug_value(Value),
          run(UpdatedVm, UpdateOffset);
        {ok, UpdatedVm, UpdateOffset} ->
          run(UpdatedVm, UpdateOffset)
      end;
    true ->
      done
  end.

run_instruction({op, return}, Vm, Offset) ->
  {Value, UpdatedVm} = update_vm_stack({pop}, Vm),
  {ok, Value, UpdatedVm, Offset + 1};
run_instruction({op, negate}, Vm, Offset) ->
  {{value, Value}, Vm0} = update_vm_stack({pop}, Vm),
  Vm1 = update_vm_stack({push, {value, -Value}}, Vm0),
  {ok, Vm1, Offset + 1};
run_instruction({op, positive}, Vm, Offset) ->
  {{value, Value}, Vm0} = update_vm_stack({pop}, Vm),
  Vm1 = update_vm_stack({push, {value, +Value}}, Vm0),
  {ok, Vm1, Offset + 1};
run_instruction({op, binary, Op}, Vm, Offset) ->
  {{value, Value0}, Vm0} = update_vm_stack({pop}, Vm),
  {{value, Value1}, Vm1} = update_vm_stack({pop}, Vm0),
  Vm2 = update_vm_stack({push, {value, erlang:(Op)(Value0, Value1)}}, Vm1),
  {ok, Vm2, Offset + 1};
run_instruction({op, constant}, Vm, Offset) ->
  #vm{ chunk = Chunk } = Vm,
  Value = chunk:get_instruction(Chunk, Offset + 1),
  {ok, update_vm_stack({push, Value}, Vm), Offset + 2}.

interpret(Chunk) ->
  Vm = #vm{ chunk = Chunk },
  run(Vm).

test() -> 
  Chunk = chunk:start_chunk(),
  Chunk1 = Chunk#chunk{instructions = [
    {op, constant}, {value, 1}, {op, negate},
    {op, constant}, {value, 2}, {op, positive},
    {op, binary, '+'},
    {op, return}
  ]},
  interpret(Chunk1).
