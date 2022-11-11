-record(vm, {
  chunk = none,
  ip = none,
  stack = []
}).

-record(chunk, {
  instructions = [],
  constants = []
}).