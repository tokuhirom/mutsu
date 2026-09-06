# Native methods still without a declared set of accepted named arguments

**Narrowed again 2026-09-07 (slice 2).** The mechanism is
[ADR-0070](../../docs/adr/0070-native-methods-declare-the-named-arguments-they-accept.md);
slice 1 built it and slice 2 drained the measured residue. See
`news/2026-09/native-method-accepted-nameds-slice-2.md`. What is left is a
short, *measured* list, none of which is a straightforward "add a row".

## The measurement

`scripts/native-method-adverb-sweep.raku` is the progress metric: 1 422
(receiver, method, argument-shape) probes derived from
`src/builtins/native_method_row_table.rs`, each comparing `R.M(A)` against
`R.M(A, :qqzz9)`. Run it under the interpreter you want to measure.

| | not named-blind, of 1 422 |
|---|---|
| mutsu before slice 2 | 78 |
| mutsu after slice 2 | **18** |
| `raku` baseline (calls Rakudo itself rejects) | 23 |

`comm`-ing the two `-v` listings is the useful view: only **9** of mutsu's 18
are mutsu-specific.

## What is left

### 1. `subst` and `trans` — a real `%_`-read set, not a signature

Both read adverbs out of a slurpy, so the Rakudo *signature* survey is only a
lower bound for them and they are deliberately undeclared:

- `subst` declares `*%options` (the survey now names the slurpy, so this case is
  visible rather than silent).
- `trans` declares nothing but the implicit `*%_` **and still reads `:d`, `:s`
  and `:c` out of it** — measured, and the reason "the only slurpy is `%_`"
  cannot be used as a rule.

Declaring either needs its full adverb set established by hand (roughly
`match`'s set for `subst`: `:g/:global`, `:x`, `:nth`/`:st`/`:nd`/`:rd`/`:th`,
`:i`, `:m`, `:s`, `:ov`, `:ex`, `:c`, `:p`, plus `:samecase`/`:samespace`/
`:samemark` and `:squash`/`:complement`/`:delete` for `trans`). Neither
currently answers a `:qqzz9` probe wrongly, so this is hardening, not a live
bug — but it is the one place where getting the row wrong would break a real,
widely used adverb, so it wants its own slice.

### 2. Constructors (`new`) are deliberately undeclared

`Blob.new(1,2,3).new(:qqzz9)` answers `Blob.new(0)` where raku answers
`Blob.new()`. A row for `new` would be wrong in general: a constructor takes
arbitrary nameds (`Foo.new(:a(1))`), and the declaration is consulted at
entries that a native `new` shares with those. Fixing this needs the `new`
handlers themselves to distinguish an attribute-initialising named from a
positional, not a table row.

### 3. Plain-call divergences the sweep surfaces (not named-argument bugs)

These show up in the sweep only because mutsu's *plain* answer already differs
from raku, so the `:qqzz9` arm cannot agree with it either:

- `(1,2,3).grep()` / `Any.grep()` / `Any.first(1)` — raku dies "Cannot resolve
  caller grep(List:D: )"; mutsu answers the receiver (and then reports
  `X::Adverb` for the adverb arm, which is right for a resolvable call).
- `Any.values(:qqzz9)` answers `().Seq` where the plain call answers `$()`.
- `"/tmp".IO.link()` — raku dies "Too few positionals passed"; mutsu has no
  arity check on the native `IO::Path` arms. The same gap makes
  `"/tmp".IO.sibling()` answer `"/".IO` instead of dying.
- `{ $_ }.returns(:qqzz9)` — raku rejects the adverb on `Code.returns`
  (no `%_` at all); mutsu answers `Mu`. In the "too lax" direction, and the same
  is true of `of`, `WHAT`, `HOW`, `DEFINITE` and `clone`, which raku also
  rejects. mutsu implements none of those rejections.
- `4.roots(2)` answers `(2e0, -2e0)` where raku answers the two complex roots.
- `(1,2,3).rotor("b")` answers `().Seq` where raku dies "cannot unbox to a
  native integer"; `(1,2,3).AT-POS("b")` answers `Nil` where raku dies.
- `(1,2,3).splice(1, 1)` reports `X::Immutable` where raku reports
  "Cannot resolve caller".

## Affected files

- `src/builtins/accepted_nameds.rs` (the table)
- `scripts/native-method-adverb-survey.raku` (accepted-name evidence; now
  reports each named slurpy BY NAME, which is what separates the implicit `*%_`
  from a declared, read `*%options`)
- `scripts/native-method-adverb-sweep.raku` (the progress metric)
- `t/native-method-accepted-nameds.t` (123 assertions; passes under `raku`
  unmodified)
