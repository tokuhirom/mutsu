# One call resolves its multi once, not three times

[#8697](https://github.com/tokuhirom/mutsu/issues/8697), partially closed: a `where`-constrained
`multi` candidate had its constraint block run **four times for a single call**, where rakudo runs
it once. That is a semantic divergence as much as a cost — a `where` is arbitrary user code that may
count, log or memoize — and the `where` is the expensive half of a bind attempt.

It is now **two**, and the case where the constraint *rejects* is at parity with rakudo.

## The issue's own guess was wrong

[#8697](https://github.com/tokuhirom/mutsu/issues/8697) reasoned that the repetition had to come
from the ten `choose_best_matching_candidate` call sites that `dispatch_resolve.rs` and
`dispatch_proto_call.rs` spread across a fallback cascade. `rust-gdb` backtraces on the repro say
otherwise: **one user-level call resolved the multi three separate times**, from three independent
VM paths, all of them inside `dispatch_func_call_inner` and all funnelling into
`resolve_function_with_types`:

1. `find_compiled_function_memo` → `find_compiled_function_inner` → `resolve_function_multi_cached_sym`;
2. `push_multi_dispatch_frame_sym` → `push_multi_dispatch_frame_with_winner_sym`, which was passed
   `None` for the winner and so resolved it itself;
3. a third, direct `resolve_function_with_types`, run purely to read the winner's declaring package
   and hand it to the deprecation check.

A breakpoint costs no rebuild, which is the whole reason `CLAUDE.md` puts `rust-gdb` ahead of
`eprintln!`: three backtraces answered in one run a question that reading the code had produced a
confident wrong answer to.

## Why three resolutions cost *user code*

For an ordinary multi none of this would matter: `resolve_function_multi_cached_sym` answers from
`func_multi_resolve_cache` and the registry walk happens once. But it **bails out to a fresh
resolve** when `func_multi_dispatch_type_cacheable` refuses the family, and that gate refuses any
family carrying a `where` clause, a literal-valued parameter, a **subset-typed parameter**, a
coercion, an `is rw` trait, or a constrained `&`-sigil parameter — everything whose winner depends
on the argument's *value* rather than its type. For those, each of the three resolutions ran the
full candidate walk and every candidate's `where`.

That gate is also the answer to a question
[#8696](https://github.com/tokuhirom/mutsu/issues/8696) left open: what its step 2 calls "cache the
type-based narrowing" is precisely this refusal, and it is why that issue measured subset-typed
families growing linearly while plain-type families stayed flat.

## The fix was an API that already existed

Path 1 already kept its answer in a per-call memo — `multi_def_memo`, added by
[#7573](https://github.com/tokuhirom/mutsu/issues/7573) and extended by
[#7886](https://github.com/tokuhirom/mutsu/issues/7886) for exactly this reason; its doc comment
states that a second resolution re-runs user code. Paths 2 and 3 sit in the same function, between
that memo's declaration and an existing reuse of it further down, and simply did not read it.
`push_multi_dispatch_frame_with_winner_sym` was written for this and says so in its own doc
comment ("told which candidate is being called instead of resolving the name a second time"); it
only needed handing the winner. Path 3 takes the memo with a fallback to resolving when it is empty,
which is the compiled-key-cache-hit case that returns before the memo is filled.

Both sites are on the `compiled == Some(cf)` branch, mutually exclusive with the
`multi_def_memo.take()` further down, so reading the memo here cannot disturb that.

## Measured

`multi f(Int $x where { $c++; $x > 0 })` against `multi f(Int $x)`:

| case | before | after | rakudo |
| --- | --- | --- | --- |
| the `where` matches | 4 | **2** | 1 |
| the `where` rejects | — | **1** | **1** |

`rust-gdb` now shows exactly one entry to `choose_best_matching_candidate` per call.

Three full candidate walks per call becoming one is also a 3x cut for every value-dependent multi,
which is the family #8696 measures. On its 2,000-call probe, against `raku` v2026.07 on the same
box:

| candidates | original | after #8696 step 1 | after this | vs raku |
| --- | --- | --- | --- | --- |
| 5 | 0.2330 s | 0.1783 s | **0.0516 s** | 80x → 60x → **17.8x** |
| 20 | 0.8194 s | 0.3808 s | **0.1373 s** | 281x → 130x → **47x** |
| 80 | 3.7929 s | 1.4237 s | **0.4692 s** | 1274x → 478x → **156x** |

3.03x from this change alone — which is the three-resolutions-to-one ratio, as it should be — and
**8.1x** cumulatively at 80 candidates.

**The empirical exponent is still ~+0.8.** This removes a constant multiplier of three; it does not
make a repeat call free, so the order gap #8696 step 2 names is untouched. Wall-clock figures for a
document must still come from the bench CI; these compare binaries on one box.

## What is left, and why it was not forced into this change

The remaining second evaluation in the matching case is **not a resolution at all**. It is the
winning candidate's actual parameter bind re-checking a constraint the resolution had already
proved — which is exactly why the rejecting case is already at 1 (nothing binds) and only the
matching case is at 2. Closing it means carrying "already validated" from resolution into the
binder, and that has its own correctness surface: which argument the verdict belongs to, which
candidate, and what invalidates it. #8697 stays open for that.

`t/routines/dispatch/multi-candidate-ranking.t`'s `where`-count assertion tightens from a bound of
4 to a bound of 2. It stays a *bound* rather than rakudo's 1 for two reasons: the remainder above,
and because it also pins that the per-registry-key dedup
[#7858](https://github.com/tokuhirom/mutsu/issues/7858) added — which #8696 step 1 moved ahead of
the ranking loop — keeps working, so the count cannot climb back toward once-per-registry-key.
