# A block's closing brace at end of line now terminates the statement

In Raku a `}` that closes a block and sits at the end of a line is a statement
separator. Whatever starts the next line begins a new statement — even when it
is spelled like an infix operator:

```raku
g { 1 }
before { 2 }      # two calls, NOT g({ 1 } before { 2 })
```

mutsu's parser had no such rule, so the expression parser cheerfully consumed
the `before` on the next line as `infix:<before>` and folded the two statements
into one comparison.

This is the shape `Cro::HTTP::Router` uses for the middleware stanza of a
`route { }` block:

```raku
my $app = route {
    before-matched { $before-m-p.keep }
    before { $before-p.keep }
    after { $after-p.keep }
    after-matched { $after-m-p.keep }
}
```

`before-matched`'s block was joined to the next line's `before` block by
`infix:<before>`, so the call received a single `Bool` argument and died with
"No matching candidates for proto sub: before-matched". `after-matched` on its
own was fine — only `before` and `after` are also infix operators, which is why
the failure looked so arbitrary.

## Mechanism

Rakudo implements this with a `$*ENDSTMT` dynamic variable that its `ws` rule
consults. mutsu's parser has no single whitespace chokepoint the infix layers
share, so `parser::stmt_ending_brace` takes the mirror-image approach: the
block-term parser records **where the next token after such a brace starts**
(as a `(pointer, length)` pair identifying the exact input position, so a mark
cannot alias a position in another buffer), and the operator recognisers ask
`infix_barred_by_stmt_ending_brace` before consuming an operator there.

The mark is set only when a newline (optionally after a trailing `# comment`)
separates the brace from the next token, so a brace in the middle of a line
still takes an infix: `say ({ 1 } before { 2 })` is unchanged. It is set only
for a block parsed as an expression *term*, not for the body of an `if` / `for`
/ `sub` declaration, so `}` followed by `else` on the next line is unaffected.

## Result

Cro's `t/http-middleware.rakutest` no longer aborts after its 11th subtest: it
runs all 24, with 22 passing. (Two remain: subtest 4's
`Cro::HTTP::Middleware::RequestResponse` and subtest 22's "After middleware is
applied", both separate issues.)

Pinned by `t/block-brace-ends-statement.t`, which checks the positive case, the
same-line case that must still parse as an infix, and the two cases where a
newline or a mid-line brace must not terminate anything. All five assertions
match `raku`'s behaviour exactly.

## Known adjacent gap

A paren-less call to a sub whose name is an infix word (`sub before(&c) {…};
before { 2 }`) still parses as a bare `before()` plus a stray block, because
`is_infix_word_op` refuses to treat any of those names as a listop regardless of
whether a sub of that name is declared. Cro's own code is unaffected (its
`before` calls sit inside a `route { }` block where the imported sub wins), so
this is recorded rather than fixed here.
