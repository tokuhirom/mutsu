# An anonymous *parametric* `role Name[...] {...}` literal is misparsed as an expression term

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/Metamodel/ParametricRoleGroupHOW.rakudoc:21`).

**Refines** [anon-role-expression-term-parse-fail.md](anon-role-expression-term-parse-fail.md)
(filed from a sibling batch-4 run against `Type/Metamodel/ParametricRoleHOW.rakudoc`),
which described *both* the parameterless `(role NAME {...})` and parametric
`(role NAME[...] {...})` forms as hard parse errors. Re-verified directly on current
`main`: the parameterless form already parses and runs fine on its own (`(role Zape2
{}).HOW.say;` succeeds); the doc's combined two-statement example only *looked* like
both forms failed because mutsu parses a whole file before running any of it, so the
first statement's real parse failure (the parametric form, below) aborts the entire
file before the second, parameterless-form statement ever gets a chance to run. This
ticket narrows the root cause to the parametric `[...]` signature specifically.

## Root cause

mutsu's expression-position parser recognizes `role Name {...}` (no parameter list) as
a term — e.g. `my $x = role Zape {}; say $x;` works and prints `(Zape)`. But as soon as
the role declaration carries a parameter list (`role Name[...] {...}`), the same
construct in expression position is *not* recognized as a role-literal term at all.
Instead the parser backtracks to treating the bare `role` keyword as an ordinary
bareword term (satisfying `my $x = <term>`), and then tries to parse the remaining
`Name[...]` as a *separate* expression — which misfires because `Name` starting with
`Z` (or any identifier) followed by `[` is grammatically ambiguous with Raku's
`Z<infix>` list metaop syntax (zip-with-operator, e.g. `Z+`), so `Zape[...]` gets
parsed as the metaop `Z` with a custom infix operator named `ape`, applied to
`[...]` as its right operand.

`--dump-ast` on the minimal repro confirms this:

```
$ target/debug/mutsu --dump-ast -e 'my $x = role Zape[Int $n] {};'
...
Expr(
    MetaOp {
        meta: "Z",
        op: "ape",
        left: DoStmt(VarDecl { name: "x", expr: BareWord("role"), ... }),
        right: BracketArray(...),
```

Note this reproduces with any parameter list, not just a type-capture (`::T`) one —
`role Zape[Int $n] {}` fails the same way.

## Minimal repro

```raku
(role Zape[::T] {}).HOW.say;
```

or, isolated further:

```raku
my $x = role Zape[Int $n] {};
say $x;
```

- `raku`: `Perl6::Metamodel::ParametricRoleHOW.new` / prints the role type object.
- `mutsu` (`target/debug/mutsu`): `===SORRY!=== ... Confused. expected statement:
  expected use statement or import statement or no statement or need statement or unit
  statement or ...` for the first form, and `Unsupported reduction operator: ape` for
  the second.

Note that the *statement-level* declaration form already works fine —
`role Zape[::T] {}; say Zape;` parses and runs correctly. Only the anonymous
expression-term form (RHS of `=`, argument position, etc.) is affected.

## Affected files (starting point)

- The parser's term-position handling of the `role`/`class` keyword (grep for where
  the parameterless `role NAME {...}` expression-term case is already handled — the
  parametric `[...]` signature needs to be recognized there too, the same way it
  already is for the statement-level `role` declaration parser).
