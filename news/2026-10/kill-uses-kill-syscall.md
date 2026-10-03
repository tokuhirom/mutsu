# `kill` calls kill(2) instead of spawning `kill`

The `kill` routine used to run the `kill` binary from `$PATH` for every signal, costing a
fork/exec each time and breaking when `$PATH` was empty or shadowed. It now calls `libc::kill`
directly (native builds on Unix), so delivery works with an empty `PATH`. Pids or signals outside
the `i32` range return `False`.
