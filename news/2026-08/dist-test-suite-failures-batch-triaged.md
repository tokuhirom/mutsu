# The dist test-suite-failures batch is fully triaged

`scripts/dist-compat-sweep.py --run-tests` (n=60, seed 20260719) found 14
`test_die` dists in the sample's own-test-suite axis (28 loading dists ran
their tests: 7 pass, 5 fail, 14 die). Every row in that bucket has now been
triaged, across two rounds (2026-07-26 and 2026-08-14). One was already fixed
before triage started (`Prime::Factor`, multi dispatch honouring a named
parameter's aliases). Of the remaining 13:

**Fixed generally, each pinned by a `t/*.t` regression test:**

- `Text::Sorensen`, `P5seek`, `App::SudokuHelper` — not reproducible on
  current main; fixed as side effects of unrelated work.
- `String::Splice` — a spurious octal warning for a bare word inside `<...>`.
- `Locale::Dates` — invoking a user class as a coercion (`Locale::Dates($locale)`).
- `Date::YearDay` — qualified constructor calls (`self.Date::new(...)`) and
  subclass-aware Date arithmetic.
- `PSpec` — a closure argument to a custom infix operator not writing back
  its outer-lexical mutation, and a leading `{ ... }` block never recognized
  as an infix operand.
- `Array::Rounded` — four general bugs in `is Array` subclass construction,
  `nextwith`, and fractional subscripts.
- `Math::Interval` — a sigilless `\name := $var` bind wrongly treated as
  readonly regardless of the RHS shape.
- `Random::Choice` — the entire `nqp::*_n` native-num arithmetic family
  (`add_n`/`sub_n`/`mul_n`/`div_n`/`neg_n`/`abs_n`) was unimplemented.
- `Mathematica::Serializer::Encoder` — a stray `Nil ~~ UInt => true` arm in
  smart-match type checking, dead since a generic `!value.is_nil()` guard
  already handles the intended case.
- `Crypt::RC4` — `Blob() :$key!`-style coercion of a native array argument,
  plus a compile-time `bind_vardecl` flag leaking into a nested `do`-block
  variable declaration.

**Filed as deep findings needing a design pass** (root-caused, not fixed):

- [`todo/deep/listops-are-not-real-multi-subs.md`](../../todo/deep/listops-are-not-real-multi-subs.md)
  (`String::Splice`'s unreachable imported multi/proto)
- [`todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md`](../../todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md)
  (`Array::Rounded`'s remaining `postcircumfix:<[ ]>` overload gap)
- [`todo/deep/element-itemization-lost-in-scalar-binding.md`](../../todo/deep/element-itemization-lost-in-scalar-binding.md)
  (`Math::Interval`'s list-destructuring sigilless bind)
- [`todo/deep/mark-context-flags-leak-across-live-call-boundary.md`](../../todo/deep/mark-context-flags-leak-across-live-call-boundary.md)
  (`Crypt::RC4`'s remaining "Cannot modify an immutable Range")
- [`todo/deep/trait-mod-does-not-callable-and-no-variable-mop.md`](../../todo/deep/trait-mod-does-not-callable-and-no-variable-mop.md)
  (`Hash::Restricted`'s dynamic `trait_mod:<is>` role mixin onto a variable)
- [`todo/deep/p5tie-container-protocol-and-array-parse-bug.md`](../../todo/deep/p5tie-container-protocol-and-array-parse-bug.md)
  (`P5tie`'s missing container-binding protocol, plus an unrelated parse bug)
- [`todo/deep/sigilless-alias-assignment-skips-type-constraint.md`](../../todo/deep/sigilless-alias-assignment-skips-type-constraint.md)
  (`Native::Overflow`'s type constraint skipped through a sigilless alias)

**Filed as its own ticket:**

- [`todo/tickets/sigilless-constant-invisible-in-nested-sub-inside-module.md`](../../todo/tickets/sigilless-constant-invisible-in-nested-sub-inside-module.md)
  (`RSV`'s sigilless constant invisible to a nested sub in a non-unit module)

No row in the original bucket remains un-triaged, so the batch ticket
(`todo/tickets/dist-test-suite-failures-batch.md`) is retired.
