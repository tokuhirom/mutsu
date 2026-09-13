# The `<subrule>` call that needs no activation

Round 21 of [#7576](https://github.com/tokuhirom/mutsu/issues/7576). Instructions
on the 60-row YAMLish document go **1,238,570,142 -> 1,199,253,834 (-3.17%)**.
Attribution is callgrind `Ir` throughout, cache warmed first (`tmp/prof60.sh`,
the round-15 harness).

Round 20 left left-recursion bookkeeping as the largest single cluster at ~3.6%:
three thread-local hash-map operations per `<subrule>` call — begin the
activation, ask whether its seed was consulted, tear it down — on a grammar with
no left recursion at all. Every previous round made those operations *cheaper*
(one map instead of three, an interned key instead of three `String` clones, one
probe instead of two). This round asks the prior question instead: does the call
need an activation?

## What the activation is for, and when nothing can use it

An activation exists so that a re-entry of the same `(rule name, arguments,
position)` key reads a growing seed instead of recursing forever. So it is dead
weight exactly when nothing can re-enter the key while it lives — and a key is
re-entered only by a call to a rule of the *same name*.

The rule call graph already decides that. `regex_call_graph::reenter_decline`
walks from `(package, rule name)` and reports whether a call to that name is
reachable; the streamed `<subrule>` path (ADR-0073 Slice 2) has consulted it
since #7548. Round 20's note named the two things that stopped it being a slice,
and both turn out to be answerable:

**The key carries no package while the analysis is per package.** It does — but
the walk's comparison is on the callee's NAME alone (`callee.1 == name_sym`),
which is precisely the question a package-blind key asks. What the walk cannot
see is an *enclosing* activation of the same name in another package, since that
call is not in this call's cone. So the gate also requires that no activation of
the name is live anywhere on the stack, which is now an array index rather than a
lookup: `LrState` keeps a per-`Symbol::id` count of live activations next to the
key map, bumped and dropped by `lr_begin_activation` / `lr_end_activation`.

**A wrong verdict fails as unbounded recursion, not as a wrong answer.** True,
and it is why the second precondition is new. The walk deliberately treats an
embedded `{ ... }` / `<?{ ... }>` block as harmless — it names no edge, and both
callers keep a runtime escape for the case where user code re-enters the rule by
hand (the eager arm's `first_only` retry, the streamed path's `seed_consulted`
fallback). Skipping the activation skips the very escape that covers it. So the
walk now reports a second fact alongside the edges — does any rule in the cone
run user code at all (`{ ... }`, `<?{ ... }>`, `:my $x = ...`, a `** { ... }`
quantifier bound, a `<:prop(...)>` predicate) — and the gate requires `false`.

With all three conditions met the skip is not an approximation: in every state
where it applies, the three map operations it replaces would have created an
entry, read `false` out of it, and removed it again.

## What it is worth

`std::thread::LocalKey::with` goes **83,245,564 (6.72%) -> 63,769,904 (5.32%)**
inclusive, and the two callers round 20 named are the whole of the difference:

| caller | before | after |
| --- | ---: | ---: |
| `for_each_atom_candidate` (the streamed subrule path) | 22,169,959 Ir / 134,850 calls | *nothing left to show* |
| `regex_match_atom_all_with_capture_in_pkg_inner` (the eager arm) | 21,984,162 Ir / 130,045 calls | 4,396,376 Ir / 24,946 calls |
| the gate itself (`subrule_needs_no_lr_bookkeeping`) | — | 6,910,900 Ir / 86,473 calls |

The gate is memoized per `(package, rule name)` per token generation — 41 cone
walks for 86,473 asks — and its cache miss is `#[inline(never)]` so the probe
that answers every other call stays a few instructions. It does not take a
registry read lock to rule out a custom-HOW grammar, either: both call sites
already know that answer (the streamed path declines on it outright, the eager
arm binds it for its own dispatch attempt), and asking again cost an `RwLock`
acquisition per `<subrule>` call for a fact that is not even memoizable.

## Pinning it

`t/regex/regex-lr-bookkeeping-gate.t` is twelve rows of the constructs that have
to keep the gate shut — a directly left-recursive rule, a mutually recursive
pair, a `{ ... }` block that re-enters its own rule name at the same position, a
`<?{ ... }>` assertion, a `** { ... }` quantifier bound, a side-effecting block
whose firing order is observable — plus the plain grammar whose results must not
change and the rule name shared by two grammars. A widened gate would not give a
wrong answer on any of them; it would recurse until the stack ran out, so every
row is also a does-it-terminate row. The ten rows rakudo can run (it has no
growing-seed loop and hangs on a left-recursive rule) were verified against
rakudo 2026.07.

Two Rust unit tests in `regex_lr_state` cover what Raku cannot reach
deterministically: a live activation of the name closes the gate and reopens it
on teardown, a new token generation retires every verdict, and the re-entry
branch of `lr_begin_or_reenter` does not count as a second activation — counting
it would leak a live activation and wedge the gate shut for the rest of the run.

## What the next round starts from

Re-measured after this round rather than carried forward:

- **Allocator traffic is now the largest cluster by a distance**: `malloc`
  5.02%, `_int_free` 6.41%, `_int_malloc` 3.18%, `free` 2.95%, plus `memcpy`
  2.46%. With `CapStore::merge_delta` at 1.89% self and `RawTable::clone` at
  1.44%, this is the capture store's design pass that rounds 15-20 have each
  deferred — it is not a slice, and it is the only thing left that is worth
  several percent.
- `Symbol::intern` is now the single biggest `LocalKey::with` client at
  **28,995,150 Ir (2.42%) over 209,978 interns**, diffuse callers.
- The gate's own residual, 0.78%: 2,507,881 Ir of self plus the 6,910,900 Ir
  probe. Half of it (the memoized verdict) could live on the `NamedAtom` node
  the way round 20 put the lookup spec there, but the other half — is an
  activation of this name live — is dynamic and has to read the thread-local.

Cumulative over rounds 11-21 on the 60-row document: **8.130 Bn -> 1.199 Bn
`Ir` (-85.2%)**.
