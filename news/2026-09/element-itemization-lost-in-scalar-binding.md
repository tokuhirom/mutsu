# Array/Hash elements are itemized at the store — the finding is closed

This file was `todo/deep/element-itemization-lost-in-scalar-binding.md`, open
since 2026-08 as the tracking record for mutsu's largest remaining container
divergence: raku's model is that every `Array`/`Hash` element **is** a `Scalar`
container, so an element read is one item in list context and renders itemized
(`@c[0].raku` is `$["a", "b"]`), with no parameter binding involved. mutsu stored
elements bare and compensated in one renderer.

The mechanism decision became
[ADR-0040](../../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md),
which shipped in five slices. **ADR-0040 slice 5 (2026-09-02) closes this
finding**: the ADR's own §1.3 divergence matrix — 25 rows, each a separate block
so no earlier statement can contaminate it — now produces output byte-identical
to `raku` v2026.07, as does §1.6's discriminator experiment.

## What the campaign turned out to be

The finding's own cost estimate ("a survey-sized campaign with its own fallout
class ... changes what is IN every array") was the thing that kept it deferred,
and it was **too high**. ADR-0040 §1.4 measured the post-fix state by hand
(`my @i = $[1,2], $[3,4]`) against 25 behavioural probes and got 25 identical
results plus 10 raku-exact renderer probes: the consumer surface was already
itemization-transparent, because the flag rides on the same shared
`Gc<ArrayData>` and every flattening decision point already consulted
`is_itemized()`. What was genuinely survey-sized was only the enumeration of
*store sites* — and slice 4b found even that had a single funnel per container
kind.

The five slices, in order:

1. **Slice 1** — the mutation sites (element assign, autovivification,
   push/unshift/append/prepend/splice on a real `Array`/`Hash`).
2. **Slice 2** — the construction sites (list-assign into `@a`/`%h`,
   real-container literal construction, `.Array`/`.Hash` coercion), plus the
   native `JSON::Fast` decoder by hand.
3. **Slice 3** — the reflection side: `.VAR` answers from the *source container*
   rather than re-deriving from the element.
4. **Slice 4** — the chained-subscript store. Scoped as "delete the
   compensators"; the compensators were not redundant, and the reason was a bug:
   a two-level chain's leaf, a 3+-level chain's leaf and its autovivified
   intermediates, and a deferred vivification token's walk-created container all
   stored bare.
5. **Slice 4b** — the constructor *is* the store (`Value::hash`), which caught
   the ~160 native Rust construction sites at once; both compensators deleted.
6. **Slice 5** — this sweep.

## The two corrections this file made, both upheld

- The cost estimate above.
- **The "list-destructuring bind write-through" bullet was misfiled.**
  `my (\a, \b) := ($x, $y); a = 10` does not reach `$x` because the desugar
  builds a temp array of *copies*; no element containerization could ever fix it.
  Re-verified 2026-09-02 (it now dies with `Cannot assign to an immutable value`)
  and filed on its own as
  `todo/tickets/list-destructuring-sigilless-bind-copies-instead-of-binding.md`.

## One suggestion in this file does not survive

It proposed that, once ADR-0040 landed, `ForLoopSpec::source_items_are_bare` (the
compile-time flag that decides eager-`for` topic writability) be replaced by the
same per-item `is_container_ref()` test the lazy path uses. That premise is
wrong: ADR-0040 itemizes elements — a `Scalar` wrapper or a kind tag — it does
**not** promote them to `ContainerRef` cells (that is ADR-0036/ADR-0013
territory), so the runtime test the lazy path uses still would not fire on an
eager element. Measured instead: of the nine topic-writability rows, eight now
match `raku` exactly, and the ninth (`for %h { $_ = 9 }`, which raku rejects) is
already tracked by `todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md`,
whose blocker is ADR-0036, not this.
