# A method frame with nothing to write into its env no longer copies the shared empty overlay

A call frame's fresh env overlay shares the process-wide empty tier and copies it
on the first write (`Env::cow_mut`, one allocation). The compiled method fast
path asked for the env mutably (`env_mut()` plus `reserve`) before it knew
whether any parameter had to be written, so since the plain scalar parameters
left the env (#15850, #15854) every call of a method whose parameters are all
read through their slots still forked the empty tier for nothing. The write set
is now decided first (`fast_param_in_env`), and a frame with nothing to write
never asks.

On a no-parameter method called 100,000 times, allocations went from 3 to 2 per
call and instructions from 1,206.4M to 1,162.4M, about 440 per call (second
callgrind run of each, profiling build, `origin/main` against this branch).
The remaining two allocations are the frame's own `scoped_child` and the
samewith context's name `String`.
