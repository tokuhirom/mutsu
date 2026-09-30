# A streamed `.map`/`.grep` sets its loop up once, not once per element

A `for` loop over a deferred `.map`/`.grep` pulls one element per iteration
(#9936), and so does `pull-one` on its `.iterator` (#10186). Each pull used
to run the whole inline map/grep loop over a one-element slice. That meant
re-deciding whether the callback needs the full call path, cloning its body
AST for tail normalization, reclassifying every captured name, building the
save/restore key list as `String`s, and walking the body AST again to decide
that the native rw loop does not apply. All of that is a function of the
callback alone, and each element paid for it.

The setup is now an `InlineLoopPlan` (`src/runtime/map_grep_plan.rs`) that
holds the compiled body, the capture classification, the interned temporaries
and parameter names, and the loop's topic flags. It is built once and kept in
the Seq's `SeqSource::MapGrep::plan` slot. A pull then pays only the env swap
(`enter_inline_loop_env` / `leave_inline_loop_env`), the bind and the body. The
bulk loops (`eval_map_over_items`, the rw map, the grep loop and the batched
`.first`) build the same plan in a throwaway slot, so the four loops share one
setup implementation instead of four hand-kept copies. Several cheap fixes
help both paths:

- `sub_is_call_carrier` and `bind_loop_topic` use pre-interned symbols.
- The tail normalization runs only on a compile-cache miss.
- The per-Seq prefix-pullability, native-rw and callback-package checks are
  cached in the slot.
- A `for` loop takes its next streamed value without allocating a chunk
  vector.

callgrind, profiling build, 5000 elements, Ir per element after subtracting
the empty-program baseline:

| workload | before | after |
| --- | ---: | ---: |
| `for @big.map(*+1).eager { ... }` | 7.7k | 6.3k |
| `for @big.map(*+1) { ... }` | 23.7k | 11.3k |
| `for @big.map({ $_ + 1 }) { ... }` | 21.2k | 10.3k |
| `for @big.lazy.map(*+1) { ... }` | 20.8k | 10.9k |
| `for @big.grep(* %% 2) { ... }` | 25.8k | 13.5k |
| `@big.map(*+1).iterator` drained with `pull-one` | 71.1k | 59.0k |

The streamed loop is now 1.79x the eager one, down from 3.1x. #10187's goal
is 1.3x, so it stays open. Most of what remains is the per-pull env swap
(about 1.7k Ir, from the cost of individual `Env` operations), the nested
register frame (about 0.7k) and moving each pull's result through a `Value`
array (about 1k).
