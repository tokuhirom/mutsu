# A method call's env merge no longer boxes its iterator

`merge_method_env` (the merge of a method frame's writes back into the caller's
env, run by every method call that makes calls or writes) chose between the
frame's write log and its whole overlay with a `Box<dyn Iterator>`, one heap
allocation per merging call. It now uses a two-variant iterator enum.

`benchmarks/method-call.raku` (callgrind, profiling build, second run, same
base): 188,424 to 178,415 allocations (-5.3%, 10,009 fewer: one per
`distance-to` call), 551,814,576 to 550,813,226 instructions (-0.18%). The
close condition of #16098 (176.7k allocations) is now within 1%; the
instruction side (523.5M) is still +5%. `Refs #16098`.
