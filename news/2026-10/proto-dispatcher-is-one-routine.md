# A multi dispatcher is spelled as its proto, and the qualified and imported ones agree

BinaryHeap's `t/03-exports.rakutest` checks that an imported `&heapsort` "is short for"
`&BinaryHeap::Utils::heapsort`:

```raku
sub is-proto(&got, &expected, $desc = '') {
    is-deeply (&got, |&got.candidates), (&expected, |&expected.candidates), $desc;
}
```

The module declares `our proto heapsort(|) is export(:DEFAULT, :heapsort) {*}` inside
`package BinaryHeap::Utils`, with two `multi` candidates. mutsu failed the check — and the ticket
([#10532](https://github.com/tokuhirom/mutsu/issues/10532)) read the failure as "the import is a list of the
proto and its candidates". It is not: both sides build that same list. The two sides differed in
three smaller ways, which together made the `is-deeply` fail:

- **`&Pkg::proto.candidates` was empty.** `&Pkg::name` evaluates to a by-name `Routine` handle
  whose `name` already spells the whole registry key (`BinaryHeap::Utils::heapsort`) and whose
  package is only the current one, so `routine_candidate_subs` looked up
  `GLOBAL::BinaryHeap::Utils::heapsort/…` and found nothing. A qualified name now spells the key
  by itself.
- **A dispatcher was spelled as one of its bodies.** `&hs.raku` gave `sub hs { #`(Sub|27) ... }` and
  the qualified handle `sub Pkg::hs (|) { #`(Sub|0) ... }`; rakudo spells a multi's dispatcher as its
  proto, `proto sub hs (|) {*}`, whichever way it is reached. `Interpreter::dispatcher_raku` renders
  exactly that: the declared proto's own signature (`proto sub g ($a) {*}`), or the generated one
  (`(;; Mu |)`) for a `multi` written without a proto. A `.candidates` entry is spelled
  `multi sub hs (...)`, and the dispatcher answers `.is_dispatcher` (it was `False`).
- **The two dispatchers were not `eqv`.** The imported `&hsort` is a `Sub` carrying its captured
  candidates; the qualified reference is a by-name `Routine` handle. `eqv` had no arm for that pair,
  so `(&got, |…) eqv (&expected, |…)` failed on its first element. A dispatcher `Sub` and a handle
  now compare equal when they name the same package-qualified routine.

## What was tried and dropped

The first version made `&Pkg::proto` the same dispatcher `Sub` the import is, by letting
`resolve_code_var` and `multi_candidates_over` look candidates up under a qualified name. That
fixed `.cando` and `.package` too, but it routed re-exported protos through
`register_package_code_alias` (`runtime_module_exports.rs`), which then registered only one of two
candidates (`t/modules/import-export/trait-mod-is-export-symbol.t` caught it: `proto-routine('x')`
reached the `@values` candidate). That path needs its own fix first, so the qualified reference
stays a handle and the rest is filed as [#10707](https://github.com/tokuhirom/mutsu/issues/10707):
a dispatcher's `.signature`/`.arity`/`.count`, `&Pkg::proto.cando`/`.package`, and `eqv` on routines as
rakudo's `.raku` comparison.

## Tests

`t/routines/proto-dispatcher-is-one-routine.t` (18 rows, all measured on rakudo 2026.07 and passing
under it): `.candidates` through both names, both `.raku` spellings, `.is_dispatcher` on a
dispatcher and on a candidate, `multi sub` for a candidate, the `is-proto` helper above, the declared
and generated proto signatures, a plain sub staying a plain sub, and a `my`-scoped proto staying
invisible as `&Pkg::name`. Against the real distribution, BinaryHeap's `t/03-exports.rakutest` test 2
now passes; its remaining failures (`heapsort` results, the empty heap's `.Bool`, the
`BinaryHeap::MaxHeap[Block]` spelling of [#10533](https://github.com/tokuhirom/mutsu/issues/10533)) are
other gaps.
