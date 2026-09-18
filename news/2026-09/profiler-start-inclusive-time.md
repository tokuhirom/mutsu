# Profiler inclusive time stays on the sampled stack

The profiler now documents the decision for `start` and `EVAL`: `incl_us` is
credited only to locations present on the thread's sampled stack. A `start`
worker is not synthetically folded into the spawning thread, and the profile
does not add a separate `spawned_incl_us` field. A normal call-site line that
is already part of the worker stack keeps its ordinary inclusive credit.
