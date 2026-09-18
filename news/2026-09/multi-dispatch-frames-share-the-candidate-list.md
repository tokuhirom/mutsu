# Multi dispatch frames share the candidate list instead of copying it

`push_multi_dispatch_frame_with_winner_sym` runs on **every** call of a `multi`
with two or more candidates. Until now it built a fresh
`Vec<Arc<FunctionDef>>` holding every candidate except the winner:

```rust
let remaining: Vec<std::sync::Arc<super::FunctionDef>> = all_candidates
    .iter()
    .filter(|c| Some(c.body_fingerprint()) != current_fp)
    .cloned()
    .collect();
```

That list exists only so `callsame` / `nextsame` / `nextcallee` have somewhere
to defer to, and the overwhelming majority of calls never redispatch. It cost
one allocation plus N `Arc` clones per call, where N is the family's candidate
count — the last O(candidates) step on the ordinary call path after
[#8696](https://github.com/tokuhirom/mutsu/issues/8696) cached the *resolution*
per argument-type key.

## What changed

`MultiDispatchEntry.1` is now a `MultiRemaining` (`src/runtime/decl_types.rs`):
a **view** over the shared, per-generation candidate list
(`MultiCandidateList = Arc<Vec<Arc<FunctionDef>>>`, memoized by
`resolve_all_multi_candidates_cached_sym`) rather than a private `Vec`:

```rust
pub(crate) struct MultiRemaining {
    all: MultiCandidateList,
    next: usize,
    skip_fp: Option<u64>,
}
```

- The winner is skipped **lazily**, by body fingerprint, while walking — so
  pushing a frame clones one `Arc` and is O(1) regardless of candidate count.
  Skipping by fingerprint rather than by index reproduces the old filter
  exactly: it dropped *every* candidate sharing the winner's fingerprint, not
  just the first occurrence.
- Advancing the chain (`advanced_past`) moves `next` instead of copying a tail,
  and the indices `iter()` hands out are absolute positions in `all`, so
  `advanced_past(i)` yields the same sequence the old
  `candidates[i + 1..].to_vec()` did.
- `lastcall` sets `next = all.len()` instead of `Vec::clear()`. This is the
  point the sharing had to get right: frames over the same family now alias one
  `Arc`, so truncating a `Vec` in place would have truncated every other live
  frame's chain too — including the recursive caller's.
- A one-candidate multi is still a dispatcher with nowhere to defer to, and
  `MultiRemaining::empty()` gives it that frame out of a thread-local shared
  empty list, so it allocates nothing either.

`callsame` also stopped deep-copying the whole candidate vector: the
`multi_dispatch_stack.last().cloned()` sites in `builtins_dispatch_next.rs`
now clone an `Arc` and two integers for that field.

The proto-candidate path (`dispatch_proto_call.rs`) and the Callable-value path
(`builtins_operators_fallback.rs`) build their own orderings, so they wrap an
owned `Vec` with `MultiRemaining::from_vec` — the fallback path's own
winner-excluding copy is gone the same way.

## Measurement

The deterministic counter (`--features alloc-stats`, `MUTSU_ALLOC_STATS=1`) on
the [#8696](https://github.com/tokuhirom/mutsu/issues/8696) probe — `n`
candidates of `subset S$i of Int where { False }` plus one `multi cand(Str $x)`,
called 2,000 times with a `Str` argument:

| candidates | allocations before | after | bytes before | after |
|---|---|---|---|---|
| 5 | 89,369 | 85,369 | 6.58 MB | 6.39 MB |
| 80 | 231,183 | 219,183 | 19.07 MB | 15.04 MB |

At 80 candidates the frame cost 6 allocations and ~2 KB of transient memory
*per call* — the `collect()` reallocating its way up to 80 elements, since
`filter().cloned()` gives `Vec` no usable size hint — and now costs none. These
counts are exact and load-independent; the saving scales with candidate count,
which is the point.

Wall-clock on the same probe at 200,000 calls (release, best of 7, interleaved):
5 candidates 0.526 s before / 0.609 s after, 80 candidates 0.714 s before /
0.631 s after. Only the *slope* is meaningful here — the ephemeral container this
was developed in has a ±20% run-to-run spread at fixed `n`, wide enough to swamp
the fixed term in both directions. That slope, what the other 75 candidates cost,
falls from 0.188 s to 0.022 s.

## Regression pin

`t/routines/dispatch/multi-shared-candidate-chain.t` covers the chain
behaviours the sharing must not disturb: a straight `callsame` walk through
three candidates, `lastcall` inside a *nested* dispatch of the same family
leaving the outer frames' chains intact (the aliasing case above), two later
dispatches of that family still resolving normally, `nextcallee` consuming the
candidate it hands back, and a single-candidate multi's `callsame` evaluating
to `Nil` rather than dying. All six agree with rakudo.

Closes [#8727](https://github.com/tokuhirom/mutsu/issues/8727).
