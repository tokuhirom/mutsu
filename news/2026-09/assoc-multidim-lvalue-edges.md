# Three associative multi-dim lvalue edges now agree with rakudo

`todo/tickets/associative-multidim-lvalue-edge-divergences.md` recorded three
independent gaps in how the associative multi-dim subscript behaves as an
lvalue. All three are closed; each turned out to be a slightly different
question than the ticket asked.

## 1. `//=` / `||=` stored where rakudo does not — and it is not a multi-dim bug

Under 6.d `%h{1;2}` is a MULTISLICE, so the lvalue is a one-element `List`:
always defined, always true. `//=` and `||=` can therefore never store, and
rakudo leaves the hash completely untouched — no autovivification either.
mutsu produced `{"1" => ${"2" => Any}}`.

The ticket blamed the *read* ("mutsu evaluates the definedness of the leaf
rather than the multislice wrapper"), but the read was already correct
(`my %h; (%h{1;2}).raku` is `(Any,)`, and `.defined` is `True`). The defect was
the **store**: `short_circuit_compound_assign_expr` implements the real
`$x // ($x = v)` short circuit — no store when the value is kept — but only for
plain named scalar targets; every subscript target kept the flat
`LHS = (LHS // v)` desugar, which writes the old value back and so autovivifies
the path.

So the fix is not multi-dim-specific, and the single-subscript twin is the proof:

```raku
my %h is default(9); %h<a> //= 7;   # rakudo {}, mutsu gave {:a(9)}
my @a is default(9); @a[3] //= 7;   # rakudo [], mutsu gave [Any, Any, Any, 9]
```

`short_circuit_subscript_assign` now gives `Expr::Index` and
`Expr::MultiDimIndex` targets the same short circuit the named-scalar path has
had, and `+=` still stores (rakudo's `%h{1;2} += 7` really is `8` — the
multislice numifies to 0).

The chain-rooted spelling (`%o<i>{1;2} //= 7`) needed one more thing: its read
answered a bare `Nil` because the whole intermediate was missing, so the
short circuit saw an undefined LHS and stored. Multislice-ness is a property of
the SUBSCRIPT FORM, not of what the container holds yet — rakudo answers
`(Any,)` for `my %o; %o<i>{1;2}`, `my $x; $x{1;2}` and `my @a; @a[0]{1;2}` alike
— so `walks_associative` now counts an undefined target, and a leaf that never
existed reads as `Any` rather than `Nil`, matching the named-root spelling.

## 2. A `Whatever` key in an associative assignment is refused

`my %h; %h{*} = 5` stringified the `Whatever` into a literal `"*"` key — a
silent write nothing detects. rakudo throws `X::AdHoc` "Cannot assign to *, as
the order of keys is non-deterministic", and does so whether or not the hash is
empty. The guard sits at the outermost index-assign entry, before the fast
paths that stringify the key.

Deliberately narrow: the READ (`%h{*}`, which lists the values) is unaffected,
a literal `"*"` key still works, and the POSITIONAL `@a[*] = 7, 8, 9` is
untouched — rakudo allows it, because an array's order is well-defined.

## 3. `:delete` on an associative multi-dim subscript does not resolve

`postcircumfix:<{; }>` has exactly two candidates, `(\SELF, @indices)` and
`(\SELF, @indices, :$exists!)`, so `%h{1;2}:delete` throws `X::Multi::NoMatch`.
mutsu accepted it and deleted the innermost key.

Two boundaries the ticket did not mention, both measured on rakudo v2026.06 and
both load-bearing:

- **It is a 6.d-and-earlier rule.** 6.e grew the candidate:
  `use v6.e.PREVIEW; %h{"a";"b";"c"}:delete` answers `42`. The refusal is gated
  on the same version test as the multislice wrapper itself, which is why
  `t/hash-multislice-container.t` (`use v6.e.PREVIEW`) keeps asserting the
  deletion.
- **A one-dimension "multi-dim" subscript is not the `{; }` form at all.** It is
  only ever produced by the `||` splat (`%h{|| @indices}`), which is an ordinary
  `postcircumfix:<{ }>` slice and does accept `:delete` — rakudo answers
  `(42, 666)` for `t/multidim-splat-lazy.t`'s spelling.

The adverb's *value* cannot rescue it either: `%h{1;2}:delete(0)` and the
dynamic `:$delete` spelling throw exactly like the bare `:delete`, because
resolution fails before the flag is consulted. That guard is keyed on which
lowering the form actually selected, not on `is_positional` — the first cut used
`is_positional` and broke 97 subtests of `roast/S32-hash/multislice-6e.t`, whose
`:$delete` loop runs the same subscript with the flag both False and True.

## One local test was asserting non-rakudo behaviour

`t/multidim-container-ref-coherence.t` (`use v6;`, so 6.d) asserted that
`%hash{"a";"b";"c"}:delete` returns 42 and mutates the hash. rakudo throws
there. The block now asserts the refusal and exercises its cross-frame-cell
coherence with `:exists`, the adverb 6.d does have.

## Coverage

`t/assoc-multidim-lvalue-edges.t` — 23 assertions, all dual-oracled against
rakudo: the short circuit for both multi-dim and single subscripts (including
the `is default` shapes, `+=` still storing, and the positional lvalue still
storing), the `Whatever` refusal with its message and its three exemptions, and
`:delete` refused for the `{; }` form while the positional, single-subscript and
`||`-splat spellings keep working. `make test` (3646 files) and a full local
`make roast` (1436 files, 218962 tests) are green.
