# A program that loads Test no longer mirrors every local store into the env

`use Test` brings an `EVAL` and a few immediate-caller readers (`CALLER::`,
`&CALLER::LEXICAL::(..)`) into every program. Both made the process-wide
`reflective_name_access_possible()` latch true, so each store to a slot-only
local was also written to the env by name (about 700 instructions a store).

Stores now ask the chunk (`CompiledCode::mirrors_all_params`: the non-`EVAL`
latch, this chunk holding or nesting an `EVAL`, or a body stashed in
`stmt_pool`). A unit whose routine EVALs counts too (the root chunk takes
`note_routines_naming_lexicals` like a sub body). An immediate-caller reader is
served when the call is made instead: `push_caller_env` copies the calling
frame's slot-only locals, whatever kind of frame it is (found through the
`VmCallFrame` the call opened, or `current_code`), into a scoped overlay on the
entries it pushes; an `EVAL` run with a `CALLER::` context overlays them for its
run and stores what it assigned back into the frame's slots. This replaces the
light-frame-only record from #15047.

Narrowing the gate re-exposed by-name readers of slot-only locals that the
latch had been masking, all of them wrong for a program that never loads Test
too. Each is now an env-sync trigger: a variable name pushed as a string value
(`cas(@p[1;1], ..)` returned 0, #15055), the container tags of `with`/`given`
element sources, aggregate `WrapVarRef` arguments (`sub f(\c) { c<b> = 1 }`
lost the caller's other keys), regex adverb arguments (`m:nth($n)` died with
`Invalid :nth index ()`), the shape dimensions of a named sub's signature
(`sub f(@a[$n])`), and a type declared inside a class body (`BEGIN` in a nested
class).

Two smaller costs that only showed once a module was loaded went too: the
lexical package of a routine frame is memoised under an epoch bumped wherever
its tables change, and `CheckReadOnly` probes the chunk's slot index once; the
mirrored store itself no longer clones the name.

Release callgrind, `sub f($n) { $n + 1 }; my $s = 0; for ^N { $s = f($s) }`,
(Ir(21000) - Ir(1000)) / 20000, second run, profiling build: no `use` 5344
Ir/call, `use Test` 5497 (was 6333 before, +18.5%; now +2.9%).
`env_deep_copies` stays 4 for the whole run.
