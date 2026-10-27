# A program that loads Test no longer mirrors every local store into the env

`use Test` brings an `EVAL` and a few immediate-caller readers (`CALLER::`,
`&CALLER::LEXICAL::(..)`) into every program. Both made the process-wide
`reflective_name_access_possible()` latch true, so each store to a slot-only
local was also written to the env by name (about 700 instructions a store).

Stores now ask the chunk (`CompiledCode::mirrors_all_params`: the non-`EVAL`
latch, or this chunk holding or nesting an `EVAL`). An immediate-caller reader
is served when the call is made instead: `push_caller_env` copies the calling
frame's slot-only locals, whatever kind of frame it is, into a scoped overlay on
the live env, which the pushed `caller_env_stack` / `callframe_stack` entries
and the callee's env chain (an `EVAL` with `context => CALLER::`) start from.
This replaces the light-frame-only record from #15047.

Two smaller costs that only showed once a module was loaded went too: the
lexical package of a routine frame is memoised under an epoch bumped wherever
its tables change, and `CheckReadOnly` probes the chunk's slot index once.

Release callgrind, `sub f($n) { $n + 1 }; my $s = 0; for ^N { $s = f($s) }`,
(Ir(21000) - Ir(1000)) / 20000, second run: no `use` 5338 Ir/call, `use Test`
5496 (was 6333 before, +18.5%; now +3.0%). `env_deep_copies` stays 4.
