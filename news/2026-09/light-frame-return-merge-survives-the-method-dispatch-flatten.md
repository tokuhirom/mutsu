# The light-frame return merge survives the method-dispatch flatten

A light-called routine that makes one full method dispatch used to pay a
scope-sized return merge for the rest of its life, however few names it actually
wrote. It now pays for its own writes and nothing else: on the ticket's own
workload the return merge drops from 2530 to 191 instructions per call, and the
whole loop from 65,533 to 50,666 instructions per iteration. Closes #7630.

## What the flatten took away

`Env::overlay_is_shared_empty()` is the ADR-0004 J4d frame-reuse latch, and the
scoped overlay it inspects is the reason the light-call unwind is cheap at all:
a scoped env's overlay *is* the list of names the frame wrote, because a fresh
tier starts empty and everything else is out of reach in the parent. So
`finish_positional_light_env` merges what is in the overlay and discards the
rest, at O(callee writes).

`flatten_scoped_env` — the guard `exec_call_method_op_impl` /
`exec_call_method_mut_op_impl` run before any dispatch past their fast paths, so
that a capture or a full-view iteration downstream is not starved of parent
lexicals — collapses the whole chain into one `parent: None` map. After it,
"the overlay" and "the whole visible scope" are the same set. The unwind cannot
tell a callee write from a caller lexical by looking at the map any more, so it
fell through to `retain_overlay` over everything visible, resolving each
`Symbol` to a string (`k.with_str(...)`) and asking `cf.is_callee_local_sym`
about it. The answer stayed correct — a caller lexical is not a callee-local, so
it survived the retain — but the work was O(scope), and one dispatch put the
frame in that state permanently: `parent.is_some()` is the latch's first
condition, and a flattened env can never satisfy it again.

## The fix: the collapse hands the writes forward

The ticket sketched three routes and validated none. The one taken is the first,
carried all the way: **`Env` records the frame's writes when, and only when,
the flatten is about to erase them.**

`Env::flattened_for_frame()` — the collapse `flatten_scoped_env` now performs —
is `flattened()` plus the tier's own key set, kept on the flat env as
`frame_writes`. Recording it is O(overlay), the same size the merge would have
been. The ticket's open question was the second half, writes made *after* the
flatten: those are logged as they happen, because `insert_sym` / `remove_sym` /
`get_mut_sym` all know the key they are touching and append it. A bulk edit that
cannot be logged key-by-key (`retain`, `values_mut`, `retain_overlay`) drops the
log instead of leaving it stale, which costs only the full scan this replaces.
Every other env carries `None` and pays one predictable branch per write: a
scoped tier IS its own log, and nothing else has a frame to record.

The three unwinds then read the log where there is one —
`finish_positional_light_env`, its typed cousin `finish_light_env`, and
`call_compiled_function_fast`'s scoped-path merge. The reused-frame arm becomes
`Env::retain_frame_writes`, which replays exactly the `retain_overlay` predicate
over the logged names and re-logs the survivors for the enclosing frame; the
swap-path arm walks the log instead of `overlay_iter`, looking each name up in
the flat map.

The latch itself is untouched: nothing about when a frame may be reused changes,
only what the unwind knows once the frame's env has been flattened. So this
needs no ADR — ADR-0004 J4d's scheme is what it restores, not what it replaces.

## Measurements

The ticket's workload, callgrind on the release binary with `MUTSU_JIT=off`, a
one-iteration run subtracted as the baseline:

```raku
class C { has $.n; method bump() { $!n = $!n + 1 } }
sub work($c) { my $a = 1; my $b = 2; $c.bump(); $a + $b }
my $c = C.new(n => 0);
for ^2000 { work($c) }
```

|                                    | before               | after               |
| ---------------------------------- | -------------------- | ------------------- |
| `finish_positional_light_env`      | 5,060,000 Ir (3.51%) | 382,000 Ir (0.33%)  |
| `Symbol::as_str`                   | 15,704,522 (10.88%)  | 3,467,505 (3.03%)   |
| `CompiledFunction::is_callee_local_sym` | 3,012,000 (2.09%) | 216,000 (0.19%)   |
| `Env::insert_sym`                  | 3,830,061 (2.65%)    | 1,100,187 (0.96%)   |
| `Env::flattened_for_frame` (the log itself) | —           | 146,000 (0.13%)     |
| whole loop, per iteration          | 65,533 Ir            | 50,666 Ir (-22.7%)  |

Per call the merge is 2530 Ir → 191 Ir. The ticket measured that *removing* the
guard entirely — an unsound kill switch — was worth 70,741 → 48,369 Ir per
iteration on its box; keeping the guard and handing the writes forward recovers
essentially the same ground, and the scope is no longer the multiplier.

Instruction counts are deterministic, so both columns are reproducible directly;
they are local A/B numbers for this change and not a bench-CI series.

## One behavioural difference, in the right direction

The flat-path merge used to apply its "per-frame private name" filter to every
name the flatten had swept up, which meant a light call whose body dispatched a
method deleted the *caller's* `?FILE` / `?LINE` from its env along with the
callee's. Driving the merge from the log means only names the callee actually
wrote are considered, so the caller keeps its own contextual vars. That is what
the filter always meant; the flat path just could not express it.

## Pins

- `t/light-frame-latch-across-method-dispatch.t` — 19 assertions on the merge
  rules with a user-defined method dispatch in every body: a callee-local does
  not leak and does not clobber the caller's same-named lexical, a captured-outer
  write reaches the caller whether it happens before or after the dispatch, the
  topic and `$!` stay per-frame, a closure and a `MY::` lookup made after the
  dispatch still see the whole frame, and nesting and recursion are unaffected.
  It passes unchanged under rakudo.
- Four `src/env.rs` unit tests on the log itself: what
  `flattened_for_frame` records, that a post-flatten write and removal join it,
  that `retain_frame_writes` drops only the frame's own names (and leaves the
  caller's `?FILE` alone), and that a bulk overlay edit drops the log rather than
  leaving it stale.
