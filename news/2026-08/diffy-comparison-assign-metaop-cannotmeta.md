# Diffy comparison operators used as an assignment-metaop base now raise `X::Syntax::CannotMeta`

Chaining comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`, `eq`, `ne`,
`lt`, `le`, `gt`, `ge`, `eqv`, `~~`, `!~~`, `before`, `after`, `===`, `=:=`,
`=~=`, ...) and structural (non-associative) ones (`cmp`, `leg`, `<=>`,
`coll`, `unicmp`, and the range operators `..`, `..^`, `^..`, `^..^`) cannot
be the base of Raku's `OP=` assignment metaoperator: `$x OP= $y` desugars to
`$x = $x OP $y`, which only makes sense when `OP` combines exactly two
operands into a single result. A chaining comparison (`1 < 2 < 3`) and a
non-associative structural one aren't that, so rakudo rejects the metaop
with `X::Syntax::CannotMeta` — e.g. `raku -e '6 >== 2'` gives:

```
Cannot make assignment out of >= because chaining operators are too diffy
```

mutsu previously had no dedicated diagnosis for this construct: `6 >== 2`
and `6 ~~= 2` fell through to the parser's generic "Confused" error instead
of naming the actual problem. This surfaced as
`roast/S03-operators/assign.t` failing two `throws-like …,
X::Syntax::CannotMeta` assertions when run against the real, vendored
`Test.rakumod` (`MUTSU_REAL_TEST=1`) rather than mutsu's native `Test`
provider — one of the 14 files in the `vendor-real-test-module.md` campaign
whose first failing assertion was `Got: X::Syntax::Confused` where a
specifically-typed exception was expected.

The fix adds a general check across every site that consumes a comparison or
range operator during parsing (`comparison_expr_mode` and
`structural_comparison_expr_mode` in `src/parser/expr/precedence/`, plus the
four range-operator branches in `range_expr`): if the matched operator is
immediately followed by a bare `=` (not `==` or `=>`, matching the
`OP=`-adjacency convention used for every other compound-assignment form in
the parser) and the operator is diffy, it raises `X::Syntax::CannotMeta`
with the same `.meta`/`.operator`/`.reason`/`.dba` attributes and message
text rakudo produces, verified case-by-case against `raku -e '...'`. The
check is a genuine operator-category classification
(`ComparisonOp::source_spelling()`, `is_structural_comparison_op()`, and the
new `diffy_assign_meta_dba()` in `src/parser/expr/precedence/ternary.rs`),
not a hardcoded list of the two roast-tested spellings, so it applies
uniformly to every chaining and structural operator including their negated
(`!before=`) and Unicode-alias forms.

One operator was deliberately excluded: `NotDivisibleBy` (`!%%`). Rakudo
parses `6 !%%= 2` as the METAOP_NEGATE of the *compound-assignment* operator
`%%=` (`"Cannot negate %%= because assignment operator operators are not
iffy enough"`), a different and separately-tracked gap, not this
diffy-base-of-assignment case — folding it into this fix would have produced
an incorrect exception message for that input.

`t/diffy-assign-metaop.t` pins 17 operators (9 chaining, 3 structural, 4
range operators, plus the negated-smartmatch and identity-operator forms)
against the exact class, `.operator`, `.dba`, and `.message` rakudo
produces, and confirms plain (non-metaop) uses of the same operators —
including `===`/`!==`, which are themselves distinct operators rather than
`==`/`!=` plus the `=` metaop — are unaffected. `make test` and the full
`roast/S03-operators/` + `roast/S03-metaops/` suites (79 files, 18654
assertions) pass with no regressions.
