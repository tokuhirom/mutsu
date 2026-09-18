# A multi candidate is ranked once per resolution, not 2n·log(n) times

[#8696](https://github.com/tokuhirom/mutsu/issues/8696) step 1. Dispatching a `multi` whose
candidates are distinguished by `subset` type constraints cost **~7.1 million instructions per
call** — an ordinary sub call costs ~46,000 — and grew linearly in the candidate count while
rakudo's stayed flat: 31x slower than rakudo at 5 candidates, **1273x at 80**. This change removes
the redundant work inside that. It is a constant-factor fix to an order-level problem: the
O(candidates) shape is step 2's business and is untouched here.

## How the work was being multiplied

callgrind put 17.8% of the 40-candidate run in `candidate_specificity_rank_for_args`, reached about
**3,663 times per single call** for a family of 40 candidates — roughly 91 rank computations per
candidate. Two independent multipliers, in different functions:

**`sort_candidates_by_specificity` computed the rank inside the comparator.**

```rust
candidates.sort_by(|a, b| {
    let a_rank = self.candidate_specificity_rank(&a.1);
    let b_rank = self.candidate_specificity_rank(&b.1);
    ...
});
```

`candidate_specificity_rank` walks the candidate's whole declared signature, and a `sort_by`
comparator runs `O(n·log n)` times, twice per comparison — `2·n·log(n)` signature walks per sort,
424 for a 40-candidate family against the 40 actually needed. It now uses `sort_by_cached_key`,
which calls the key closure exactly once per element.

The subtlety is that the key has to reproduce the old comparator *exactly*, or the winner changes
for candidates that tie on narrowness: rank descending (hence `Reverse`), then the registration
stamp ascending, then the registry key string. That last tie-break is kept — and the key string
cloned to keep it — rather than delegated to the sort's stability, because defs built outside a
registration path all share stamp 0, and ordering those by however the caller's gather happened to
collect them is not deterministic.

**`choose_best_matching_candidate` ranked the duplicate registry keys, then dropped them.** One
`multi` is registered under several keys — the arity key `Pkg::f/1`, the typed key `Pkg::f/1:Int`,
the `__m<n>` suffixes — and the gathers collect by key, so the same `Arc<FunctionDef>` arrives two
or three times over. [#7858](https://github.com/tokuhirom/mutsu/issues/7858) added a dedup by body
fingerprint to stop each copy's `where` clause from being *run*, but it sat *after* the ranking
loop, so each copy still got a full `candidate_rank_key` before being discarded. The dedup now runs
before the loop. Which copy survives cannot change: every copy of one fingerprint is the very same
`Arc`, so they tie on every component of the key, `decl_order` included, and `retain` keeps the
first — the one the caller's `sort_candidates_by_specificity` order had already put first, which is
the same copy the old post-sort dedup kept.

## Measured

Release build, mutsu vs `raku` v2026.07 on the same box, 2,000 calls into a family of *n*
subset-typed candidates. The runtime work is fixed across rows; only the candidate count varies.
The script times its own loop, so startup and compile time are excluded.

| candidates | before | after | change | vs raku, before → after |
| --- | --- | --- | --- | --- |
| 5 | 0.2330 s | 0.1783 s | −23% | 80x → 60x |
| 10 | 0.3896 s | 0.1979 s | −49% | 115x → 66x |
| 20 | 0.8194 s | 0.3808 s | −54% | 281x → 130x |
| 40 | 1.7518 s | 0.6993 s | **−60%** | 647x → 258x |
| 80 | 3.7929 s | 1.4237 s | **−62%** | 1274x → 478x |

2.5–2.7x at the sizes where the cost matters, and the saving grows with the candidate count, which
is what a per-candidate multiplier being removed looks like.

Instruction counts on the 40-candidate case, which are deterministic and load-independent and are
what the change was iterated against:

| | before | after |
| --- | --- | --- |
| whole run | 14,271,680,637 Ir | 5,230,100,889 Ir (−63.3%) |
| per call | 7.14 M | 2.62 M |
| `candidate_specificity_rank_for_args` | 2,545,310,558 (17.83%) | 270,778,112 (5.18%) |

The rank cost itself is down **9.4x**, which is the two multipliers going away; the whole run is
down 2.7x, in line with the wall clock above.

The empirical exponent is still **≈ +0.95** (linear in candidate count) against rakudo's ≈ 0.
That is expected and is the point of splitting #8696: nothing here caches the *resolution*, so a
repeat call still gathers and ranks the whole family. Closing the order gap means caching the
type-based candidate narrowing by argument type — step 2, which is architectural and still open.
Wall-clock figures for a document must still come from the bench CI; these are the script's own
`now` readings, used to compare two binaries on one box.

## Pinned

`t/routines/dispatch/multi-candidate-ranking.t` (12 assertions, all passing under rakudo). Both
changes are order-preserving by construction, and the failure mode if either is wrong is a
**silently different winner** — another candidate's body running, or an ambiguity going unreported
— rather than an error, so every assertion reads the body that actually ran. It covers a subset
candidate against its base type and against an untyped one in both directions, narrowness across
two positionals, arity at three arities, and `X::Multi::Ambiguous` still being raised for genuinely
tied candidates.

One thing could not be tested as intended: "equally narrow candidates, the one declared first
wins" has no valid Raku spelling, because `$b` already means `Any $b`, so two such candidates are
the *same* signature and rakudo raises `X::Multi::Ambiguous` rather than picking the first. The
ambiguity assertion covers that ground instead.

## A divergence found on the way, filed separately

The file's last assertion is a **bound**, not rakudo's value: rakudo evaluates a `where` constraint
exactly **once** for one call, and mutsu evaluates it **four times**. That is pre-existing —
verified by building the parent commit and measuring the same 4 — and is now
[#8697](https://github.com/tokuhirom/mutsu/issues/8697). The cause is one level above this change:
`dispatch_resolve.rs` and `dispatch_proto_call.rs` reach `choose_best_matching_candidate` from ten
call sites in a fallback cascade, and each gathers and binds its own list, so the per-key dedup
above — which only ever sees one gather — cannot remove repetition *across* them. It matters beyond
cost, because a `where` clause is arbitrary user code that may count, log or memoize.

What the bound pins is that the #7858 dedup keeps working now that it has moved: the count cannot
climb back toward once-per-registry-key. It tightens to 1 when #8697 lands.
