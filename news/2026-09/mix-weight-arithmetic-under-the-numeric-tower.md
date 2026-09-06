# Mix weights combine under the numeric tower, in one place

`<a b c> (+) (a => 2.5, b => 3.14).Mix` rendered `Mix(a(3.5) b(4.140000000000001) c)`
where Rakudo gives `Mix(a(3.5) b(4.14) c)`. The ticket that found this
(`Language/operators.rakudoc:1795`, 2026-09-06 doc-diff sweep) blamed `infix:<(+)>`
for coercing both weights to `f64` before adding, and pointed at
`src/vm/vm_set_ops.rs`. The operator was the right suspect; the coercion was not
one it introduced.

## Root cause

`MixData` stores every weight as a bare `f64` (`HashMap<String, f64>`). That is
not in itself the bug — a Raku `Mix` weight is a `Real`, and mutsu already has a
canonical *decoding* of the stored double, `value::mix_weight_to_value`: `Int`
for a whole number, `Rat` for one that round-trips through its shortest decimal,
`Num` otherwise. Roughly twenty read-out sites already go through it, which is
why `$m<b>.^name` correctly answered `Rat` and `(b => 3.14).Mix.raku`
round-tripped.

The defect was that the weight *arithmetic* did not use that decoding. Every
combination site did raw `f64` work — `*e += v` for `(+)`, `lv * rv` for `(.)`,
`v - bv` for `(-)`/`(^)`, `vals.iter().sum::<f64>()` for `.total` — so
`3.14f64 + 1.0` produced the double *next to* `4.14` rather than `4.14` itself,
and the decoding then faithfully reported the wrong number. Only weights whose
sum is exact in binary (`2.5 + 1`) escaped, which is why one entry of the repro
looked right.

The fix puts the promotion rule in exactly one module, `builtins/mix_weight.rs`:
decode both weights, apply the ordinary numeric tower (`Int` + `Rat` stays an
exact `Rat`; only a genuine `Num` operand makes the result a `Num`), re-encode.
A non-finite operand has no rational decoding and falls back to the plain `f64`
operation, which is what the tower would compute anyway. `Bag`/`BagHash` counts
are `BigInt` and never enter this module — their weights are `Int` and stay
`Int`, which the new test pins as a control.

Call sites converted: `exec_set_addition_op` and `apply_set_addition` (the two
copies of `(+)`), `apply_set_multiply` (`(.)`), `set_diff_values` (`(-)`),
`set_sym_diff_values` and `set_sym_diff_multi` (`(^)`, pairwise and multi-arg),
the duplicate-key accumulation used while a Mix is built from a pair list, and
`Mix.total`. The Unicode spellings (`⊎`, `∖`, `⊍`, `⊖`) share those
implementations, so they were fixed with them.

## What the neighbourhood sweep turned up

Exploring outward from `(+)` found three more defects, all fixed here because
each was another copy of the same "how is a stored weight interpreted?" rule:

- **`Mix.total` was worse than `(+)`.** It reconstructed a rational from the
  `f64` sum with its own `f64_to_rat` helper (a byte-for-byte duplicate of the
  one in `methods_narg/base.rs`) whose loop broke as soon as the scaled value
  was within `1e-10` of a whole number. So `(a => 1.00000000001).Mix.total`
  returned `1`, and `(a => 1/3).Mix.total` returned `0.33333333333333326` — a
  value that is not even the stored weight. It also always returned a `Rat`,
  where Rakudo returns an `Int` for a whole total. `.total` now sums through the
  tower and decodes the result the same way every other read-out does, and the
  duplicated helper is deleted.

- **Two renderers carried a third copy of the rule, and it saturated.** Both
  `value/display.rs` and `runtime/utils/gist.rs` printed a whole weight with
  `w as i64`, so `(a => 2e300).Mix` rendered as `Mix(a(9223372036854775807))`.
  Both now call one `mix_weight::render`, so a weight is printed the way it is
  read back.

- **`[(+)]` did not parse at all.** Every other set-operator reduction spelled
  fine; baggy addition alone died with `Confused. expected statement` before
  execution, in both the `(+)` and `⊎` forms. Two lists had simply been missed:
  the reduction-term operator set in
  `parser/primary/misc/reduction.rs` and the builtin-reduction set in
  `vm/vm_misc_ops.rs::is_builtin_reduction_op` (without the second, the freshly
  parsing `[(+)]` then died at runtime with `Unknown function: infix:<(+)>`).
  The fold itself was already implemented — `exec_reduction_op` listed `(+)` in
  its set-operator family and `apply_set_addition` did the work — so the two
  additions were enough.

- **A set-operator reduction ignored its operands' containers.** `[(^)] $b1, $b2`
  over `$`-held Bags returned `Set.new(Bag, Bag)` — each Bag treated as one
  opaque element — while the identical `$b1 (^) $b2` was correct. `[(|)]`,
  `[(&)]`, `[(-)]` and `[(.)]` were wrong the same way, and the Mix flavour of
  it was how the multi-arg `(^)` divergence first showed up. A `rust-gdb`
  breakpoint on `set_type_level` decoded the operand's NaN-boxed word as kind 43
  = `ContainerRef`: the reduction's existing `deitemize_element` pass strips a
  `Scalar` but leaves a `ContainerRef` alone, and a plain lexical read produces
  the latter, so the Set/Bag/Mix classification saw level 0. The infix opcodes
  were unaffected because they receive already-dereferenced stack values. Set
  operators now decontainerize their reduction operands.

## Known remaining divergence

A weight that cannot be decoded from its double — `(a => 1/3).Mix` — still reads
back as a `Num` where Rakudo keeps the `Rat`, because the storage is an `f64`
and `0.3333333333333333` is not a shortest-decimal round trip of `1/3`. Removing
that needs `MixData` to hold `Value` weights, which reaches roughly 474
`ValueView::Mix` match sites; it is a separate campaign, not a slice of this
one. The tower fix is exact for every weight the storage can represent, which is
every decimal literal.

## Pin

`t/mix-weight-numeric-tower.t` — 40 tests covering `(+)`/`(-)`/`(.)`/`(^)` and
their Unicode spellings, `Mix`/`MixHash` receivers, negative and zero weights,
duplicate-key accumulation, `.total` (value and type), the reduction-operand
fix, `[(+)]`/`[⊎]`, the renderer, and a `Bag`-weights-stay-`Int` control. All 40
also pass under Rakudo v2026.07.

Two further findings from the same sweep were left as tickets rather than
squeezed into this diff, since each has a different root cause:
`todo/tickets/baggy-addition-panics-on-a-bigint-bag-weight.md` (the baggy
operators flatten `BagData`'s `BigInt` counts to `i64`, so `(a => 10**30).Bag
(+) (a => 1).Bag` panics in a debug build and wraps in a release one) and
`todo/tickets/set-operator-reduction-one-arg-rule-does-not-coerce.md` (a
one-operand set reduction returns the operand instead of coercing it — wrong
identically for every set operator, `[(+)]` included).
