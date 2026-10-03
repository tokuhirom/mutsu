# ADR-9170: A closure capture shares its scope's system names as layers instead of copying them

- **Status**: Accepted (2026-10-03; implemented with the decision)
- **Related**: [ADR-0092](0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md)
  (the capture is a fallback tier of the call frame; this ADR makes that tier a
  stack), [ADR-0094](0094-closure-capture-kept-set-is-not-narrowed.md) (the
  kept set is not narrowed; this ADR keeps it whole and stops copying it),
  [ADR-0086](0086-builtin-dynamics-are-not-closure-capture-material.md) (the
  same structural move for the built-in dynamics)
- **Addresses**: [#9170](https://github.com/tokuhirom/mutsu/issues/9170), the
  `MakeAnonSub` / `MakeAnonSubParams` / `MakeLambda` / `MakeBlockClosure` /
  `MakeGather` rows

## 1. Context

`Interpreter::capture_closure_env` keeps the closure's free variables plus every
visible *system name*: anything that is not a plain user lexical. That covers
types and packages, constants, dynamics, uppercase-initial lexicals, `?`
pseudo-lexicals and `__mutsu_*` metadata. ADR-0094 decided that the kept set is
not narrowed. A name the body does not mention cannot be proven unneeded
without a free-variable analysis that does not exist for bare-word type reads.

The kept set was built as **one flat map per closure creation**. For the
plain user lexicals, #9246 had already made the walk O(free variables) by
probing names. The system names were still copied one by one. A program that
declares N classes, constants or dynamics paid O(N) on every `-> { }`, every
`{ ... }` passed as a block, and every `gather` it evaluated. The one-entry
capture memo (`vm_capture_cache`, #7624) could hand back the previous map only
while every tier of the creating chain stayed at the same address. Any
by-name write (`my $q = $_` in the loop body) moved the tier and missed the
memo.

Measured on `main` (release, `MUTSU_JIT=off`): 20000 closure creations in a
loop, with N declarations in scope. Each cell is the time at N = 1000 → the
time at N = 2000.

| N of | `-> { $q }` | `gather { take 1 }` |
| --- | --- | --- |
| `my class` | 0.40 → 0.62 s | 0.39 → 0.70 s |
| `constant` | 0.62 → 1.37 s | 0.70 → 1.35 s |
| `my $*d` | 1.07 → 1.91 s | 0.95 → 2.14 s |
| `my $Upper` | 0.33 → 0.63 s | 0.39 → 0.75 s |

## 2. Decision

A system name's binding changes rarely, so the capture shares it instead of
copying it.

1. **Each env tier memoizes its system names** (`Tier::capture_sys`). The memo
   is an immutable tier of the kept names with their values. Every `Tier`
   mutator that can change a memoized entry drops it. A write to a plain user
   lexical does not, and neither does a write to a *volatile* system name
   (`symbol::flags::CAPTURE_VOLATILE`: `_`, `@_`, `%_`, `$!`, `$/` and its
   capture views). For those the memo records only that the key is present,
   and a capture reads the live value by name. The tier's map is private, so
   this invalidation is complete by construction, as for the existing
   `capture_candidates` memo.
2. **A capture is an own tier over shared layers** (`Env::layered_capture`).
   - The own tier holds the free variables (probed), the volatile names, and
     every kept name of the narrow tiers at the top of the creating chain. A
     narrow tier is under 32 entries: a call frame's own `self`, `?CLASS` or
     `@_`. Those are cheaper to copy than to memoize, and the closure then owns
     them outright, which keeps the common closure cycles traceable by the GC.
   - Each remaining tier contributes its memo as a shared layer.
   - A creating chain that runs over a capture fallback (a closure created
     inside a closure body) contributes that capture's layers below its own.
   - The closure's own parameters and locals that shadow a system name (a
     WhateverCode's `_`, a block's `@_`) are *hidden* from the layers. The flat
     copy expressed the same thing by leaving them out
     (`CompiledCode::capture_hidden_set`).
3. **The capture fallback is a `CaptureView`**: a stack of `Layer`s, each with
   an optional hidden set. A call frame installs the capture's own tier over
   its layers (`Env::capture_view`) in O(layers), where it used to install one
   `Arc<Tier>`.
4. **Iteration sees the whole capture.** Consumers that walk a `SubData`'s
   env by iteration were written against a flat copy that held everything.
   These are the substitution replacement block, END phasers, the `xx` thunk
   and where-constraint merges. A flat env with a fallback is a layered
   capture, and nothing else has that shape, so its `iter` / `keys` /
   `values` / `len` show the folded view, built on first ask and cached
   (`Env::iter_tier`). A call frame (a scoped env) still iterates its own
   overlay only. Mutators that must see every entry (`retain`, `values_mut`,
   by-value `into_iter`) fold the capture into a plain flat env first.
5. **Precedence is the flat copy's.** Inside the capture, the creating chain's
   layers resolve above the base tiers (`GLOBAL_BASE`, the built-in
   dynamics), as the copied names did (`Layer::above_base`). Layers that were
   already a capture fallback resolve below them, as the flat copy's base
   exclusion had it. A call frame consults its whole capture below the base,
   which is ADR-0092 unchanged.
6. **The GC traces only what the capture owns.** `SubData` and `LazyList`
   trace the own overlay, plus the folded iteration view when it exists and
   is held only by that env. A shared layer is an external holder in the same
   sense a shared overlay already was. A cycle routed only through a wide
   tier's system names is therefore deferred (under-collected), never freed
   wrongly.
7. **The capture memo is retired.** `vm_capture_cache` existed to avoid
   rebuilding the flat copy. The layered capture costs O(f · d + n + l) to
   build:
   - f free variables, d chain depth;
   - n entries of the narrow top tiers, each tier under 32;
   - l layers.
   That leaves the memo nothing to save, and its tier-pinning cost
   (copy-on-write on the next by-name write) nothing to pay for.
   `Env::tier_addrs` / `tier_maps` go with it.

A gather's force no longer folds its captured env (`Env::flattened` treats a
flat env with a fallback as already flat), and its write-back merge walks only
the env's own overlay, where every write of the body lands.

## 3. Consequences

- Closure and gather creation stop scaling with the system names in scope.
  The table in §1 becomes flat: 0.040 → 0.039 s for classes, and 0.07 → 0.07 s
  for gather.
- A frame installing a layered capture allocates a `CaptureView` per call,
  where it used to bump one `Arc`.
- A tier whose system names do change in a loop rebuilds its memo per capture,
  as the flat copy rebuilt its map. An example is a loop that writes `my $*x`
  every iteration. That is no worse than before.
- Out of scope: a `state` declaration makes a loop over its chunk cost O(state
  locals) per iteration (`sync_state_locals_in_range`). This was filed as
  [#11347](https://github.com/tokuhirom/mutsu/issues/11347).
