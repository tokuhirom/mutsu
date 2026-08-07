# A sub named after an infix word can never be called as a listop

`is_infix_word_op` (`src/parser/primary/ident/predicates.rs`) lists every name
that is also an infix operator — `Z X R x xx eq ne lt gt le ge cmp coll unicmp
leg and or not div mod gcd lcm but does min max ff fff before after andthen
orelse notandthen` — and the identifier parser refuses to treat any of them as a
listop call. That is right for an undeclared name, but it holds even when a sub
of that name is in scope, so a paren-less call to it is mis-parsed:

```raku
sub before(&cb) { say "called" }
before { 2 };            # mutsu: calls before() with no args, leaves { 2 }
                         #        dangling -> "Too few positionals passed"
                         # raku:  calls before({ 2 })
```

With parentheses (`before(5)`) it works, so the gap is specific to the listop
(paren-less) form.

`Cro::HTTP::Router` exports subs named exactly `before` and `after` and its
tests call them paren-less inside a `route { }` block. That happens to work
today, so this is not currently blocking anything — but the same shape in any
other module would fail.

## Where to fix

The identifier parser should consult the declared-sub table before applying
`is_infix_word_op`: a name that is a *declared routine in scope* and appears in
term position (i.e. not directly after a complete operand) is a listop call, not
an infix. Note the two halves are already distinguished elsewhere — the same
predicate is consulted from term position and from operator position, and only
the operator-position use should be unconditional.

Care is needed for the genuinely ambiguous case `@a min @b` where a user has
also declared `sub min`, and for `x`/`xx`, whose infix form is extremely common.

## Related

Found while fixing the "block brace at end of line ends the statement" rule
(`news/2026-08/block-brace-at-end-of-line-ends-the-statement.md`); the two
interact, since the terminator rule is what makes the next line's `before` a
term position at all.
