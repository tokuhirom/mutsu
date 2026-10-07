# A trivial typed `loop` body costs 43% fewer instructions

The loop from #12151 (`my int $orig = $value; $value = $orig + $i; last if $value == $orig + $i;`
inside `for 1..N -> int $i`) cost 17.8k instructions per iteration under callgrind
(711.7M for 40k iterations). It now costs 7.7k (308.4M), measured on the same
profiling build family, warm, with `valgrind --tool=callgrind`.

What was removed, in the order it landed:

- `==` on two `Int` (or two `Num`) operands returns before the junction, user-candidate and
  numeric-bridge machinery, as `<` already did (-1.5k).
- A typed declaration no longer re-interns its name three times, and the per-scope saved
  environment (`loop_local_saved_env`) is keyed by `Symbol` instead of `String` (-1.6k).
- A plain scalar `my` whose slot the compiler proved `!needs_env_sync` takes an env-free lane
  (`vm_decl_lane.rs`): `SetVarDynamic`, `SetVarType*` and `SetLocalDecl` write the slot and
  skip the name-keyed half of the dual store that nothing reads. The declared-type
  metadata is still registered, since the full store path reads it by name (-2.0k typed,
  -2.0k untyped).
- A native `int`/`str`/`num` store that already matches its constraint skips the match,
  coerce and wrap steps; `TypeCheck` has a tag-test fast path for the same shape.
- `exec_one` no longer moves the large `Result` through `finish_op_result` on success.
- The JIT accepts `NativeIntArithmetic`, `Last` and `Next`, so the loop body is compiled
  instead of being rejected whole (`MUTSU_VM_STATS` showed `bailouts=2`).

Not done: the inner `loop` still costs about 1.6k per entry (scope push/pop, condition
range, handler guard), the outer `for` binds `$i` through env, and the typed-`my`
metadata is registered and removed again on every loop entry. #12151 stays open.
