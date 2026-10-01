# `use trace` prints every statement to stderr, numbered like Rakudo

`use trace` used to die with "Could not find trace". It is a core lexical
pragma: every statement compiled while it is in effect writes a header and its
own source text to the real standard error just before it runs.

```
$ raku -e 'use trace; say 1; say 2'
2 (-e line 1)
say 1
1
3 (-e line 1)
say 2
2
```

mutsu now prints the same output for the constructs checked against `raku`
(2026.09): statements of every kind, nested blocks, declarations, phasers,
`CATCH`, `EVAL` (a unit of its own), `no trace` and block-scoped use. The
parser puts a `Stmt::Trace` hook ahead of each statement the pragma governs,
the compiler turns it into one constant string and a new `Trace` opcode writes
it to stderr. Like Rakudo's `writefh(getstderr, ...)` it bypasses `$*ERR`, so a
program that rebinds `$*ERR` does not capture it.

## Why it was deep: the statement number

The leading number is Rakudo's `$*STATEMENT_ID`, bumped on *every attempt* to
parse a `statement` -- including the failed attempt that ends each non-empty
statement list at its closing `}`, the second attempt after a statement label,
every empty statement between `;;`, and the statements inside the `semilist`
of `( )`, `[ ]`, `%( )` and subscripts (but not a call's argument list, and
nothing for an empty `()`). A leading `use v6.d;` and a proto's `{*}` body take
no number.

mutsu's parser backtracks and sometimes parses a statement speculatively
*before* the statement around it, so it cannot keep such a counter. Each
attempt is recorded instead by source position (idempotent, however often a
statement is re-parsed), a hook carries its statement's position, and when the
unit is fully parsed `trace::number_statements` replaces each position with its
rank among all the attempts. The recording is only switched on for a source
that mentions `trace`, so other programs pay one substring search per parse.

The `if`/`unless` exception in the issue has a cause: Rakudo folds a
block-form `if`/`unless` on a compile-time-constant condition into the branch it
picks, and the hook goes with the folded statement. A constant-*false*
condition with an `elsif` chain is not folded. `with`/`without` and statement
modifiers never are. The parser applies the same rule to literal conditions
(`trace::folds_away`).

A `Stmt::Trace` is a sibling of the statement it traces rather than a wrapper,
so declarations stay at the top level of their lists, and the shape checks that
recognise a stub body `{...}` or a proto dispatcher now look past it through
`Stmt::is_marker`.

## Known differences

- The statements inside a regex code block (`/ a { 1 } /`) are not numbered:
  the block is parsed from a copy of its text ([#10669](https://github.com/tokuhirom/mutsu/issues/10669)).
- The number of times a block runs is mutsu's, so a trace shows what mutsu
  executes: a pure `sort` comparator block is not called at all, and `min`/`max`
  with a block call it a different number of times than Rakudo's algorithm.
- A role body's statements (and so their traces) are run once more at class
  declaration time than Rakudo runs them; that predates this pragma
  ([#10667](https://github.com/tokuhirom/mutsu/issues/10667)).
- A bare statement in a parametric role's body, which a trace hook is, takes the
  role-composition route that re-binds type parameters, so the class in
  `t/oo/role/qualified-role-multi-concretization.t` resolves to another
  concretization under `use trace` ([#10679](https://github.com/tokuhirom/mutsu/issues/10679)).
- Constant conditions made of constant *expressions* (`if 1 + 1`, `if ?1`)
  are folded by Rakudo but not recognised by the trace rule.

Tests: `t/modules/import-export/use-trace-pragma.t` (every expectation taken from `raku` and
the file passes under it unchanged).
