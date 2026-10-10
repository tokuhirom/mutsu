# callframe(N) counts subs reached through the light call paths

The positional-light and fast call paths skip `push_caller_env`, so a sub
reached through them left no `callframe_stack` entry and `callframe(N)` from a
callee two subs deep answered the outermost call site (#12266). `callframe`
now merges `callframe_stack` with the routine frames those paths do push on
`routine_stack`: every unclaimed routine frame becomes a synthesized entry
whose line and file are the recorded call site. `CallFrameEntry` gained a
`routine_depth` field to tell the two apart. The synthesized frames expose no
lexicals (`.my` is empty); the hot call paths are unchanged.
