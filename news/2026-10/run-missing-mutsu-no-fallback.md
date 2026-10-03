# `run` no longer falls back to the current interpreter for paths ending in `mutsu`

When spawning failed, `run` retried with the running interpreter if the program name merely ended
with `mutsu`, so `run "/nonexistent/mutsu", ...` silently succeeded. The name-suffix heuristic is
gone: a missing program now reports a failed spawn (exit code -1) as in Rakudo. `run $*EXECUTABLE`
is unaffected.
