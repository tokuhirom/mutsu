# RoutineStack keeps the lexical-package count instead of scanning

[#8918](https://github.com/tokuhirom/mutsu/issues/8918). `running_module_bareword`
(`src/vm/vm_var_get_ops.rs`) and `get_env_with_main_alias_inner`
(`src/vm/vm_env_helpers.rs`) each opened by asking "does any live frame on the
routine stack carry a `lexical_package`?", and answered it with
`self.routine_stack().iter().rev().any(|frame| frame.lexical_package.is_some())`
— a walk of the whole stack on every call. `running_module_bareword` alone is
entered 295,917 times in a ten-decode `JSON::Fast` profile, and the walk grows
with nesting depth on a path that runs per bareword read.

## The fix

[`src/runtime/routine_stack.rs`](../../src/runtime/routine_stack.rs) wraps
`Vec<RoutineFrame>` in a `RoutineStack` that keeps `lexical_package_frames`, a
count of live frames with `lexical_package.is_some()`, maintained by `push`,
`pop`, and `truncate` — the only three ways the stack can change. `Interpreter`
now answers the question with `any_lexical_package_frame()`, an integer
compare instead of a scan.

The type deliberately implements `Deref<Target = Vec<RoutineFrame>>` for
read-only access (`len`, `last`, `iter`, indexing) but **not** `DerefMut`: a
`&mut Vec` would hand back `push`/`pop`/`truncate` on the raw vector, past the
counter. The mutation sites are spread across nine modules (`dispatch.rs`,
`dispatch_proto.rs`, `resolution_call_sub.rs`,
`builtins_operators_fallback.rs`, `regex_token_resolve.rs`,
`seq_helpers/smart_match.rs`, `accessors_stack.rs`, and the two constructors
in `runtime_init.rs` / `runtime_thread.rs`) — with the field private, the
compiler finds every one instead of relying on a convention no one can audit.
`truncate` counts only the dropped tail (`O(dropped)`, not `O(depth)`), since
those are exactly the frames whose contribution needs to go.

Unit tests in the same file assert the count against the scan it replaces
after every push, pop, and truncate call, including the no-op truncate (`len`
at or above the current depth) and the empty-stack case.

## Measured

Paired A/B re-baselined from the current `main`, both sides a fresh
`--profile profiling` build, `tmp/jf-decode.raku 200 9` (the issue's own
repro), each side's first run after the build discarded:

| | before | after |
|---|---:|---:|
| total instructions | 26,834,757,310 | 26,825,658,610 (**-0.03%**) |
| `running_module_bareword` self cost | 7,038,335 | 6,877,933 (-2.3%) |

The total is inside this box's noise floor (~2%), and that is an honest
report, not a rounding trick: `jf-decode.raku`'s records are a flat list, so
the routine stack at the bareword-read site is shallow (~3-4 frames) and the
old scan was already cheap in absolute terms here. What changed is the
*shape* of the cost, not just its size — the walk is now `O(1)` regardless of
how deep the stack gets, where before it was `O(depth)` on a path entered
hundreds of thousands of times per decode. A workload with deeper nesting
(the "`parse-thing` -> `parse-obj` -> `parse-thing`" recursion the issue
describes) would show a larger effect; this benchmark's shape does not
exercise it.

Not closing [#8918](https://github.com/tokuhirom/mutsu/issues/8918) — the
issue's own scope is the maintainer's call.
