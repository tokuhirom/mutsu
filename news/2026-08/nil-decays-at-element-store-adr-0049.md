# `Nil` decays to the container's default at the element store, and stops being a hole sentinel (ADR-0049)

A Raku `Array`/`Hash` element is a `Scalar` container, and a `Scalar` cannot hold `Nil`: assigning
`Nil` to one restores the container's own default (`Any` for an untyped element, the declared
element type object for a typed one, a native zero, or a declared `is default(...)` value). mutsu
used to implement that rule at roughly twenty scattered assignment sites and miss it everywhere
else — every array/hash construction path, `push`/`append`/`unshift`/`prepend`, autovivification,
and `.DELETE-POS` all left a raw, undecayed `Nil` sitting in a real container element. Worse, the
value that should be impossible in a real element — `Nil` — was simultaneously mutsu's sentinel for
"absent hash key", "deleted array slot", and "autovivification gap", while `Package("Any")` was the
*intended* gap marker everywhere else. `[Nil,1][0]:exists` answered `False` (raku: `True`) purely
because the stored `Nil` was misread as a hole; `[Nil,].elems` was `0` (raku: `1`) because one
construction path silently dropped a bare `Nil` element outright.

The investigation started from an RSV (`from-rsv`) module failure that traced down to
`my @b = [Nil]; @b eqv [Nil]` answering `False` where raku says `True` — a one-liner reached via
about fifteen rounds of bisection from the original dist failure. That finding grew into
[ADR-0049](../../docs/adr/0049-nil-decays-to-the-container-default-at-the-element-store.md), which
measured 29 divergent behaviors and 13 already-correct invariants against real `raku`, decided the
fix belongs at the element *store* (not the read side, and not the renderers, both of which mutsu
had already tried and found insufficient), and phased the work into six independently-landable
slices.

**What changed, end to end:**

- Every real-container construction path (`[...]`, `{...}`, `%(...)`, `Array.new`/`Hash.new`
  including the parameterized `Array[T]`/`Hash[K,V]` forms, list-assign coercion) now decays a
  literal `Nil` element to the container's own default as it is built — applied per-container, so a
  nested literal decays inside-out (`[[Nil]] eqv [[Any]]` is now `True`, matching raku).
- Element assignment, autovivification, and every array mutator
  (`push`/`append`/`unshift`/`prepend`/`splice`) now share one `Interpreter::typed_container_default`
  -derived decay rule instead of roughly twenty independent hand-rolled ladders, several of which
  disagreed with each other or missed a container's own `is default(...)` value entirely. Along the
  way, two real bugs were found and fixed: `append`/`unshift`/`prepend` were silently bypassing their
  own decay logic via a VM fast path that didn't know `is default(...)` was a thing to check for, and
  `prepend` was missing the decay call outright.
- `Nil` is retired as a hole sentinel. `ArrayData::initialized` — an embedded bitmap that already
  existed and already precisely tracked "was this index explicitly assigned" — is now the SOLE hole
  discriminator; `ArrayData::hole_at`'s old, imprecise `Some(ValueView::Nil) => true` arm is gone. A
  temporary `debug_assert!` in that arm, run across the full local `t/` suite and a broad roast sweep
  per the ADR's own recommended completeness check, caught three independent pre-existing bugs in the
  hole-tracking machinery that the old, unconditional `Nil` arm had been silently masking (a
  zen-slice `@a[]:delete` and a `Rat`-indexed `@a[1.5]:delete` both failed to trim trailing holes; a
  nested multi-index `.DELETE-POS` lost its array's type/hole metadata on rebuild) — all three fixed
  before the arm was deleted.
- `.AT-KEY` on a missing hash key now answers the container's default (`(Any)`, a typed value's
  element type object, or a declared `is default(...)` value) instead of a raw `Nil`, matching every
  other hash-key reader in the interpreter.
- Both render-side compensators for the old bug — a `Nil`-shortcut in `.raku`'s per-element renderer
  and an equivalent one in `.gist` — are deleted as dead code now that the invariant they were
  papering over holds at the store.

Two known-narrow gaps were investigated and deliberately left open rather than force-fixed: the
`is-default`-vs-read-chokepoint bug (`resolve_array_entry` substituting a container default for ANY
in-range `Package("Any")` element regardless of `initialized`, not just this ADR's `Nil` sites) and
a multidim `:exists` adverb predicate that is blind to `initialized`/typed gap markers for a
`Whatever`/list-index target — both recorded as their own tickets rather than folded in, since
neither is actually caused by (or requires touching) the `Nil`-decay mechanism itself. A related
`.splice` gap (inserted values are never type-checked against a declared element type at all,
`Nil` or otherwise) was also found and ticketed separately.

`t/nil-element-store-decay.t` pins all 29 divergence rows and all 13 invariant rows the ADR
measured against real `raku`; only row 29 (the deferred `resolve_array_entry` follow-up) stays
`todo`-marked. `t/nil-list-holes.t` and `t/typed-array-hole-adverbs.t` — the hole-model regression
net named by the ADR's own slice-0 acceptance oracle — stay green unchanged throughout.
