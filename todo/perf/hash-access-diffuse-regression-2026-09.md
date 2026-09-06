# `hash-access` / `bench-hash` drift upward ~2%/day, with no single culprit commit

The bench CI shows `hash-access` and `bench-hash` (both series, JIT on and off)
at a raku ratio of 0.19 from 2026-09-01 onward, against 0.17 through late
August. This note records what a callgrind bisect actually found, so the next
person does not repeat it.

## The 2026-08-27 "step" is runner noise, not a regression

`mutsu_median_s` for `hash-access` shows a sharp jump on 2026-08-27 at 13:26Z,
from 0.0277 (`7c60c853f`) to 0.0350 (`94a218779`) — +26%, which looks exactly
like a bad commit landing. It is not. The **raku** baseline in the same rows
stepped identically (0.1824 -> 0.2153), and the two commits are one docs change
and one `:=` bind fix. Building both and counting instructions confirms it:

| commit | Ir (`MUTSU_JIT=off`, callgrind) |
| --- | --- |
| `7c60c853f` | 219,375,414 |
| `94a218779` | 219,319,562 |

Identical to within 0.03%. The runner simply got slower that afternoon and
stayed slower. **Read the ratio column, not `mutsu_median_s`** — that is what it
is for. This mistake cost two release builds; do not repeat it.

## The real regression is a slow, diffuse creep

Instructions retired for `benchmarks/hash-access.raku`, one release build per
point:

| commit | date | Ir | step |
| --- | --- | --- | --- |
| `7c60c853f` | 2026-08-27 | 219,375,414 | — |
| `f76addaee` | 2026-08-29 | 229,047,499 | +4.4% |
| `a0932728` | 2026-08-31 | 237,598,221 | +3.7% |
| `8fff1596d` | 2026-09-02 | 246,480,320 | +3.7% |
| `d4a63aebb` | 2026-09-06 | 266,157,741 | +7.9% |

That is roughly +2%/day, sustained, with no step large enough to bisect to. It
is death by a thousand cuts: each correctness fix adds a small check to a path
the hash benchmarks run 10,000 times, and none of them is individually wrong.

A per-function self-cost diff (2026-08-29 vs 2026-09-06) shows the same shape —
no single frame dominates the delta:

| function | Aug 29 | Sep 06 | delta |
| --- | --- | --- | --- |
| `nanbox::payload_op` | 6.37M | 9.82M | +3.45M |
| `LocalKey::with` | 13.46M | 16.68M | +3.22M |
| `nanbox::peek::view_kind` | 3.74M | 6.47M | +2.73M |
| `exec_index_assign_expr_named_op*` | 4.25M | 6.55M | +2.30M |
| `Value::deref_container` | 0.37M | 2.33M | +1.96M |
| malloc/free family | 33.30M | 37.36M | +4.06M |
| `list_str_needs_interpreter` (new) | 0 | 1.89M | +1.89M |

## What has been fixed

Three of these were addressable and are done (see
`news/2026-09/hash-benchmark-hot-path-probes.md`): a tag-probe fast reject in
`list_str_needs_interpreter`, gating the `__mutsu_bound_index::` probes on the
`ELEM_INDEX_META_SEEN` flag that already exists for them, and symbol-keying the
per-item `for` topic writes. Together: `hash-access` 266.2M -> 253.3M (-4.8%),
`bench-hash` 313.3M -> 300.2M (-4.2%).

That recovers roughly the last two days of the creep. The rest of it is still
there.

## What is left, and where to look next

`Symbol::intern` is still called **~155,000 times** for a benchmark with 20,000
loop iterations — ~8% of the run, almost all of it re-interning *constant*
names on hot paths. Remaining callers, by intern count:

| callers | interns | note |
| --- | --- | --- |
| `get_env_with_main_alias_inner` | 30,007 | the `&str`-keyed by-name read chokepoint |
| `exec_index_assign_expr_named_op*` | ~10,000 | still interns `var_name` several times per store |
| `reify_lazy_array_slot` | 10,000 | |
| `is_readonly` | 10,000 | `is_readonly_sym` exists; the caller has the name, not a symbol |

The pattern to apply is the one `wk::` already documents: resolve a fixed name
once per process, or intern the variable name once per operation and thread the
`Symbol` through instead of re-interning it at each helper. The hash fast path
(`try_fast_hash_element_assign`) interns `var_name` for `env().get`,
`var_default`, `is_readonly` and the shared-store commit separately; interning
once and passing the symbol would cut four to one. That needs `_sym` variants of
`var_default` and friends, which is why it was not folded into the fix above.

Separately, `hash_one` + `sip::Hasher::write` is **15.8M Ir (6%)** on
`hash-access`: user-facing `HashData` still uses std's SipHash while the env
moved to `FxHashMap` long ago. Switching would be a large, easy win — but it
trades away HashDoS resistance on *user-controlled* keys, which is a real
decision for a language runtime and needs an explicit call (an ADR), not a
drive-by change. It is also pre-existing, not part of this regression.

## Reproduce

```
cargo build --release
MUTSU_JIT=off valgrind --tool=callgrind --callgrind-out-file=/dev/null \
    --cache-sim=no --branch-sim=no ./target/release/mutsu benchmarks/hash-access.raku
```

Ir is stable to within a few thousand instructions across runs and is unaffected
by runner speed, so single runs can be compared directly — which is exactly why
it, and not wall clock, is the right instrument for this ticket.
