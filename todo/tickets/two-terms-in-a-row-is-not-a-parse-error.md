# Two terms in a row is not a parse error

rakudo rejects two adjacent terms with no operator between them; mutsu accepts
the construct and silently evaluates only one of them.

```
$ raku  -e 'my $x = 1 1;'
===SORRY!=== Error while compiling -e
Two terms in a row
at -e:1

$ mutsu -e 'my $x = 1 1;'
Useless use of constant integer 1 in sink context (line 1)
```

The same holds for `my $x = "a" "b";` and `say 1 1;` — mutsu parses the first
term, then treats the rest as a separate statement and warns about it in sink
context, exiting 0. rakudo's `X::Syntax::Confused` with the message
"Two terms in a row" is what roast tests for.

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

mutsu's statement parser recovers from a term it cannot continue by ending the
statement there, which is what makes the second term parse as a new statement.
Turning that recovery into a diagnosis means deciding, at every place a term
completes, whether what follows may legitimately start a new statement —
a statement separator, `}`, end of input, a statement modifier (`if`, `for`,
`unless`, `while`, `given`, `when`), a block, a pointy — versus another term.
Getting that list wrong rejects valid programs, so it needs the full
`make roast` as the review rather than a targeted patch.

Affected file: `src/parser/stmt/` (statement-boundary decision) and the term
parsers under `src/parser/primary/`.
