# "Two terms in a row" is only diagnosed for a bare expression statement

mutsu has the diagnosis, but only where a *statement* is a pure value expression:

```
$ mutsu -e '1 1;'          # correct
Confused. Two terms in a row
$ mutsu -e 'my $x = 1 1;'  # wrong -- rakudo says "Two terms in a row"
Useless use of constant integer 1 in sink context (line 1)
$ mutsu -e 'say 1 1;'      # wrong, same
Useless use of constant integer 1 in sink context (line 1)
```

The check lives in `src/parser/stmt/simple_expr_stmt/core.rs:216` and is gated on
`is_pure_value_expr(&expr)` — the whole statement being a literal. A `my`
initializer and a listop argument never reach it: the initializer parser stops at
the first complete term and the leftover `1` is re-read as a *new statement*,
which then evaluates and warns in sink context, exiting 0. So the failure is not
a missing diagnosis but a missing **term-boundary check at the other two sites**.

(`my $x = (1 1);` is caught, as `X::Syntax::Malformed`, because the paren
parser does reach a boundary check.)

## How it surfaced

It was hidden behind a *different* bug. `t/parse-error-multibyte-column.t`'s
third case asserted exactly this via `is_run 'my $x = 1 1;', { status => sub { 0
!= $^a }, err => rx/'SORRY'/ }`, and that hash composer was misread as a
**Block** because `body_has_placeholder_vars` did not gate its `$^`/`@^`/`%^`
scan on `depth == 1` — so the `$^a` of the *nested* `sub` forced the outer braces
to a block. `is_run` then matched its no-matcher candidate and checked nothing.
Fixing the depth gate (`news/2026-08/implicit-catch-wrapper-does-not-trap.md`)
made the assertion real, and it fails.

The pin was re-pointed at `my $x = "abc"" ;` — the ASCII counterpart of the two
multi-byte cases it sits next to, which is what that file is actually about —
so the "two terms" gap is recorded here rather than left inside an unrelated
test.

## Why it is not a one-liner

The existing check's guard list is what makes it safe, and it is long: the
continuation must not be empty, `;`, `}`, `)`, `]`, `,`, a statement-modifier
keyword (`if`, `for`, `unless`, `while`, `given`, `when`), and it must
`starts_with_unambiguous_term`. Reusing it at the initializer and listop sites
means deciding, at each, which of those exits are legitimate there — a listop
argument list has its own comma and adverb continuations, and an initializer can
be followed by a `where` clause or a trait. Getting the list wrong rejects valid
programs rather than merely missing an error, so this wants the full `make roast`
as the review.

Affected files: `src/parser/stmt/simple_expr_stmt/core.rs` (the existing check and
its `starts_with_unambiguous_term` helper, worth extracting), the `my`
initializer parser under `src/parser/stmt/decl/`, and the listop argument parser.
