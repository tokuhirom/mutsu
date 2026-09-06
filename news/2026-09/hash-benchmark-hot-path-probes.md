# Three speculative probes taken off the hash-benchmark hot path

`hash-access` and `bench-hash` had been drifting upward at roughly 2%/day since
late August. The investigation is written up in
`todo/perf/hash-access-diffuse-regression-2026-09.md`; the short version is that
there is no single culprit commit, just a steady accumulation of small checks on
paths those benchmarks execute 10,000 times. Three of them were pure waste and
are now gone.

## `list_str_needs_interpreter` rejects by tag before it clones

`c6d8041b7` (2026-09-02) put `Interpreter::list_str_needs_interpreter` on the
`~`/`eq` operand coercion, so a list whose elements define their own `.Str`
renders correctly through interpolation. Correct, but it lands on **every**
interpolated value and every `~` operand, and its first act is
`value.deref_container()` — an owned clone — followed by `descalarize()` and a
`view()`, only to answer `false` for anything that is not a list.

The surrounding code in `exec_string_concat_op` is written to exactly the
opposite discipline: its `Proxy` and `Seq` checks are deliberately tag probes,
with comments saying so ("this coercion runs on every `~`/`eq` operand, so the
common non-`Proxy` case must not even clone the value"). This call had simply
missed that rule.

It now follows it. A new `may_hide_a_stringifiable_list` tag probe answers from
the nanbox kind word alone — true for the list kinds plus the three wrappers the
scan looks through (`ContainerRef`/`ContainerView` for `with_deref`,
`HashEntryRef` for `deref_container`'s read-through, `Scalar` for
`descalarize`), false for everything else. A `false` is exactly the `_ => false`
arm of the match it replaces, so an `Int` or `Str` operand now costs one tag
compare instead of a clone, two derefs and three views.

## The `__mutsu_bound_index::` probes use the flag that exists for them

`ELEM_INDEX_META_SEEN` is a monotonic process-global flag whose doc comment
names this exact case: "Their probes run on *every* element write (`@a[i] = x`)
... each would otherwise cost a `format!` plus a `Symbol::intern`ing env
lookup." The key only appears once a program `:=`-binds an element, which almost
no program does.

Three probes in `vm_var_assign_element.rs` — in the hash fast path, the array
fast path, and the general element-assign path — built the key and looked it up
unconditionally anyway, while a fourth probe ten lines below one of them
consulted the flag correctly. All three are now gated.

## The `for` topic is written by symbol, not by string

`restore_loop_topic` and `set_loop_topic` run once per loop item and wrote the
topic through the `String`-keyed `Env::insert`/`remove`, allocating the key
`"_"` and re-interning it every iteration — and the thread-local intern cache is
a string-keyed hash lookup, which is why `Symbol::intern` reached ~8% of
`hash-access`. `wk::topic()` exists for precisely this and resolves once per
process. The same change applies to the topic's readonly marking
(`mark_readonly_sym_with` / `unmark_readonly_sym`).

Neither key latches an env metadata flag — `note_env_key` only fires for
`__mutsu_*` prefixes — so the symbol-keyed form is equivalent.

## Result

Instructions retired (`MUTSU_JIT=off`, callgrind), against `d4a63aebb`:

| benchmark | before | after | |
| --- | --- | --- | --- |
| `hash-access` | 266,157,741 | 253,289,943 | **-4.8%** |
| `bench-hash` | 313,258,959 | 300,195,256 | **-4.2%** |
| `bench-class` | 1,530,950,359 | 1,512,043,185 | -1.2% |
| `bench-string` | 568,702,466 | 565,281,004 | -0.6% |
| `word-count` | 1,036,691,782 | 1,034,415,597 | -0.2% |

The first two recover roughly the last two days of the creep. The broader wins
are the concat fast reject, which every string interpolation in every program
pays for, and the topic write, which every `for` loop does per item.

The remaining creep, the still-substantial `Symbol::intern` traffic, and the
separate SipHash-vs-FxHash question for user-facing hashes are all recorded in
the `todo/perf/` ticket.
