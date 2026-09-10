# The closure-capture memo stops arming when it never hits

`Interpreter::capture_closure_env` builds a closure's captured `Env` by walking
every visible env key and inserting the kept ones into a fresh map — O(kept env)
per closure creation, where the kept set is dominated by the ~20 built-in
dynamics seeded once at interpreter startup. [#7624](https://github.com/tokuhirom/mutsu/issues/7624)
memoized that map one entry deep so a closure literal re-created from an
unchanged scope hands back the same `Env` instead of rebuilding it.

Profiling the case the memo was *not* supposed to help — a closure created
inside a sub or method frame, whose env tiers are fresh on every call — showed
the memo was not merely inert there. It was 4.5% of the program, at a hit rate
of zero.

## Why it armed on a path it could never hit

Arming is deliberately conditional: holding an `Arc` on each source tier forces
a copy-on-write clone on the next by-name write to a pinned tier, so `wants_arm`
waits for a repeat — "did the previous capture run against the same tier
addresses and the same chunk?" — and a churning env never pays for a memo that
cannot pay off.

Tier identity is an *address* observation, and the allocator recycles addresses.
A sub frame allocates an overlay map, the closure captures, the frame returns
and the map is freed; the next call allocates the same size and glibc hands back
the very same address. `wants_arm` sees a matching chain and says yes. But
arming holds an `Arc` on that map, so it is no longer free — and the next frame
is forced to allocate somewhere else, which the armed entry then reports as a
mismatch. The memo armed on every other creation and hit on none, paying an
`Env` clone, a `tier_maps` vector and, on the next replacement, a refcount pass
over every one of the ~24 captured values.

`CaptureCache` now tracks whether an armed entry was ever handed back. Two
consecutive arms replaced without a hit put the memo into backoff, where it
stops arming and retries only periodically, so a program whose closure creation
moves from a churning scope to a stable one can still pick the memo back up. A
hit clears the count. Three unit tests pin the three states: a stable scope still
arms and keeps hitting, a chain that can never hit stops arming, and backoff
retries a handful of times rather than never or always.

The same pass pre-interned `capture_bare_callees`' `__mutsu_in_eval` probe. That
gate exists for an import alias installed by `use` inside `EVAL`, so the common
program misses it — but it was interning the literal through the thread-local
string-keyed intern cache on every single closure creation.

## Numbers

Callgrind instruction counts, which are deterministic and load-independent:

| | before | after | |
| --- | --- | --- | --- |
| `my $c = * + 1;` × 200000 | 4,637,949,516 | 3,720,984,527 | **−19.8%** |
| `sub make($n) { my $c = { $n + 1 } }` × 20000 | 1,281,932,378 | 1,173,416,159 | **−8.5%** |
| `word-count` | 1,074,118,308 | 1,069,578,572 | −0.42% |

`bench-ctor`, `bench-class`, `bench-fib`, `bench-grammar-parse` and
`bench-yaml-parse` are flat within ±0.07%. Nothing regresses.

## What is still open

The kept set itself is untouched, so a capture that misses the memo still pays
O(kept env). Reading the *call* side while measuring this answered the question
[#7557](https://github.com/tokuhirom/mutsu/issues/7557) Part B could not:
`call_compiled_closure_in_unit` installs the closure's overlay over the **live
caller env** and merges the captured entries with `entry_or_insert_sym_with` —
don't overwrite what the chain already has. Every frame chain bottoms out at the
interpreter's root env, which holds the built-in dynamics for the whole program,
so the captured copy of `$*OUT` is discarded on every call. Those 20 entries are
not a conservative approximation that happens to be harmless; they are dead
weight that is already never read.

That reframes the narrowing from "trust free-variable analysis for names it is
not trusted for" — which is unsound anyway, since a closure calling `say`
reaches `$*OUT` through a callee — into a storage question, recorded as
[ADR-0086](../../docs/adr/0086-builtin-dynamics-are-not-closure-capture-material.md):
the built-in dynamics belong in a per-interpreter never-copied base tier, the
mutable sibling of the `GLOBAL_BASE` that already holds `$*PID`, `$*SPEC` and
`$*EXECUTABLE`. The ADR records why neither `GLOBAL_BASE` (process-wide, and
these values are per-interpreter and mutable) nor `Env::scoped_child` (ADR-0084
§5: overlay-only iteration starves every consumer that reads an env as "the
visible environment") can take them, and what a per-interpreter tier has to
preserve.
