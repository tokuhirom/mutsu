# A subset-typed multi resolves once per argument type, not once per call

[#8696](https://github.com/tokuhirom/mutsu/issues/8696) step 2, the piece step 1
([#8716](https://github.com/tokuhirom/mutsu/pull/8716)) deliberately left open.
Step 1 removed redundant work *inside* a resolution and left the shape alone:
the empirical exponent stayed at ≈ +0.95, because nothing cached the resolution
itself and a repeat call still gathered and ranked the whole candidate family.
This closes that gap. On the 40-candidate probe a call costs **888,364 → 34,592
instructions (−96.1%)**, and the growth exponent against the candidate count
goes **+0.81 → +0.11**, against rakudo's −0.01.

## Why one `where` made every call pay

`func_multi_dispatch_type_cacheable` answers a question about a whole *family*:
is every candidate of this `multi` name type+arity deterministic? A single
`where` / `subset` / literal / `is rw` candidate anywhere in the family makes the
answer `false`, and the sound resolution cache (`func_multi_resolve_cache`) is
then withheld from **every** call of that name. Each call re-gathered the
candidate list from the registry, re-ranked it, and re-bound it — O(candidates),
per call, forever.

That gate is not wrong, it is just coarse. A `subset S of Int` candidate cannot
match a `Str` argument, and *that* is decidable from the argument's type alone:
no predicate has to run, and the answer is the same for every call with those
argument types. So the family-level verdict is refined to a per-argument-type
one. When every value-dependent candidate is ruled out by its **declared nominal
types** — or by arity — the winner among the candidates that remain is a pure
function of the argument types after all, and the cache applies.

This is rakudo's own decomposition, and it is why rakudo is flat: in the probe
all 80 subset candidates are `of Int` against a `Str` argument, so correct
base-type narrowing eliminates every one of them without evaluating a single
predicate.

## What makes it sound

The rules live in the new `src/runtime/dispatch_narrow.rs`, and there are two.

**Only a check that runs no user code may exclude a candidate.** A nominal test
is `type_matches_value` against a declared base type, with a `subset` chain
resolved to the type at its end; it never evaluates a `where`, a subset
predicate or a coercion. A constraint whose meaning the interpreter does not
model is not a nominal test at all — `type_matches_value` would answer `false`
for every argument and exclude candidates that do match — so an unknown name
simply declines to exclude. So do the constraints that are not about the
argument's own type: the element type of an `Int @a`, the return type of a
`&cb`, a `::`-qualified enum refinement, a coercion.

**The exclusion must be reached before any user code would have run.**
`args_match_param_types_inner` walks the positional parameters left to right,
returns at the first failure, and tests a parameter's nominal type before its
`where`. So a candidate only counts as excluded when the excluding parameter
sits at or before the first parameter whose check can run user code. Without
that rule, caching would skip a side effect the uncached path performs:
`multi f(Int $a where { $ran++ }, Int $b)` called as `f(1, "s")` runs the
`where` and *then* fails on `$b`. (Rakudo, which checks nominal types first,
does not run it at all — so mutsu is the conservative one here, and #8697 tracks
that divergence separately.)

Everything the narrowing cannot prove stays on the uncached path, so it can only
add cache hits, never change which candidate a call reaches. The per-candidate
value-dependence rule moved out of `func_multi_dispatch_type_cacheable` into a
shared `def_has_value_dependent_param` for the same reason: the refinement is
only sound if it narrows away exactly the candidates that made the family gate
say `false`, and two copies of that rule could drift apart.

## Measured

`valgrind --tool=callgrind`, release build, the 40-candidate probe (2,000 calls),
with the fixed startup cost of a zero-iteration copy of the same script
subtracted. Instruction counts are deterministic and load-independent.

| | before | after |
| --- | --- | --- |
| instructions per call | 888,364 | **34,592** (−96.1%, 25.7x) |
| full candidate walks per 2,000 calls (n=20) | 2,000 | **1** |

For scale, an ordinary (non-multi) sub call measures 6,855 instructions per call
on the same box, so a 40-candidate subset-typed dispatch has gone from ~130x an
ordinary call to ~5x.

Wall clock from a **local** probe run — the same box for both binaries and for
`raku` v2026.07, median of 3, 2,000 calls in every row with only the candidate
count varying. Local timings drift with thermals and binary layout; the
instruction counts above are the reproducible figure, and any number quoted for
PERFORMANCE.md must come from the bench CI.

| candidates | before | after | change | vs raku, before → after |
| --- | --- | --- | --- | --- |
| 5 | 0.0326 s | 0.0083 s | −75% | 17x → 4.4x |
| 10 | 0.0494 s | 0.0082 s | −83% | 31x → 5.1x |
| 20 | 0.1064 s | 0.0089 s | −92% | 56x → 4.7x |
| 40 | 0.1594 s | 0.0096 s | −94% | 98x → 5.8x |
| 80 | 0.2959 s | 0.0111 s | −96% | 168x → **6.3x** |

Least-squares slope of log(time) against log(candidates): **+0.81 → +0.11**
(raku: −0.01). One O(candidates) step is left on the call path and is not
touched here: `push_multi_dispatch_frame_with_winner_sym` still materializes the
whole candidate list minus the winner on every call, so `callsame`/`nextsame`
has something to walk (confirmed under `rust-gdb`: the frame is pushed once per
call of this probe). Making that list lazy or shared is a bounded
constant-factor item, not the order-level one this issue was about, so it is
filed separately as
[#8727](https://github.com/tokuhirom/mutsu/issues/8727).

The issue's two controls — a family distinguished by plain types, and one
distinguished by arity — were already flat and are unchanged (0.0105 → 0.0108 s
and 0.0113 → 0.0115 s at 80 candidates, both within run-to-run noise). The
subset-typed family now costs the same as the plain-type control at every
candidate count, which is the shape the two should have had all along.

## Pinned by

`t/routines/dispatch/multi-subset-base-type-narrowing.t`, 30 assertions, all
cross-checked against rakudo: a subset whose base type matches but whose
predicate rejects (and alternating between the two, so no winner leaks across
values); two subsets over the same base with different predicates; a subset over
a user class in an inheritance chain and over a role; a subset chain;
`X::Multi::Ambiguous` still raised — on every call, not answered from a cache —
for genuinely tied candidates after an arity-narrowed family becomes cacheable;
a `where` that must still run for the call it guards; four different argument
types through one family; and the `:D`/`:U` smiley still discriminating.
