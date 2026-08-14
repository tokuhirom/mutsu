# "Two terms in a row" is now diagnosed at `my` initializers and listop calls

mutsu already had a "two terms in a row" diagnosis, but it only fired when the
*whole statement* was a bare value expression:

```
$ mutsu -e '1 1;'          # correct
Confused. Two terms in a row
$ mutsu -e 'my $x = 1 1;'  # was wrong -- rakudo says "Two terms in a row"
Useless use of constant integer 1 in sink context (line 1)
$ mutsu -e 'say 1 1;'      # was wrong, same
Useless use of constant integer 1 in sink context (line 1)
```

The check lived only in `src/parser/stmt/simple_expr_stmt/core.rs`, gated on
`is_pure_value_expr(&expr)` for the whole statement. A `my` initializer and a
listop argument never reached it: the initializer/argument parser stopped at
the first complete term and the leftover term was silently re-read as a *new
statement*, which then evaluated harmlessly and only warned about sink
context, exiting 0 instead of raising a compile error.

## Fix

The core term-boundary predicates (`starts_with_unambiguous_term`,
`is_pure_value_expr`) were extracted into a new shared module,
`src/parser/term_boundary.rs`, and reused — not duplicated — at four more
sites, each layering its own legitimate-continuation exceptions on top of the
shared base:

- `src/parser/stmt/decl/my_decl_assign.rs` (`handle_simple_assign`): right
  after the RHS of a `my`/`our`/`state` initializer is parsed, covering both
  scalar (`my $x = 1 1;`) and array/hash (`my @a = 1 1;`, `my @a = 1, 2 3;`)
  declarations.
- `src/parser/stmt/decl/mod.rs` (`consume_scalar_decl_trailing_comma`): the
  same check after each sink expression in a scalar declaration's trailing
  comma list (`my $x = 1, 2 3;`).
- `src/parser/stmt/simple/io_stmts.rs` (`parse_expr_list`, used by
  `say`/`print`/`put`/`note`): extended the existing "missing comma" guard so
  a digit/quote-starting continuation is fatal rather than silently accepted
  (`say 1 1;`, `say "a" "b";`).
- `src/parser/primary/ident/listop.rs` (`parse_expr_listop_args`,
  `make_call_expr_from_listop_args`, used by general bareword/user-sub listop
  calls): the same boundary check at the point each argument loop stops
  consuming (`f 1 1;` for any listop-style call, not just the IO builtins).

Getting the guard list wrong at any of these sites would reject *valid*
programs — worse than the original missing diagnosis — so each site's
legitimate continuations were verified against real `raku` first: a trailing
comma list, `if`/`for`/other statement modifiers, `but`/`does` role mixins on
a fresh declaration, and comma/adverb-bearing listop argument lists all still
parse exactly as before. (`my $x = 1 where * > 0;` turned out to be invalid in
real raku too — it is not a legitimate continuation, so no exception was
needed for it.)

A secondary bug surfaced once the new sites started raising: the fatal
`PError::fatal(...)` calls for this diagnosis (including the pre-existing
bare-statement one) carried no source position, so the CLI rendered them as a
plain `Runtime error: ...` line instead of rakudo's `===SORRY!===` snippet
with a caret — breaking `t/parse-error-multibyte-column.t`'s third case, which
asserts `err => rx/'SORRY'/`. All six "two terms in a row" (and the adjacent
"unexpected block in infix position") call sites were switched from
`PError::fatal(msg)` to `PError::fatal_at(msg, position)`, which is a pure
metadata addition (the error message text is unchanged) that unlocks proper
`===SORRY!===` rendering with line/column, matching real raku's output.

Pinned by `t/two-terms-in-a-row-initializer-listop.t`: the two now-fixed error
cases plus nine regression guards for continuations that must keep working
(`but`, `if`/`for` modifiers, trailing comma sinks, listop commas and
adverbs).
