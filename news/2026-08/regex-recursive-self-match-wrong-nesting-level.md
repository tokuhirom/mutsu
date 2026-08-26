# `<~~>` recursive self-match is implemented

The ticket read the symptom as "returns the inner nesting level, one recursion
too deep":

```raku
my $paren = rx/ '(' <-[()]>* ')' || '(' [ <-[()]>* <~~> <-[()]>* ]* ')' /;
say '(1 + (2 x 3)) = 7' ~~ $paren;   # raku: ｢(1 + (2 x 3))｣, mutsu: ｢(2 x 3)｣
```

The real cause is simpler and worse: **`<~~>` was not implemented at all.** It
parsed as an ordinary named subrule `~~`, no candidate resolved, and the atom
just failed. With `<~~>` always failing, the second alternative collapses to
`'(' ')'` (the `[...]​*` runs zero times), so the only branch that can match is
the first one, `'(' <-[()]>* ')'` — which finds the *innermost* balanced pair.
The "off by one level" reading was an artifact of that collapse; it did not get
worse with depth because there was no recursion to be off by.

## The fix

A new `RegexAtom::RecurseSelf(Box<str>)` carries the **source text of the
enclosing regex**. The parser knows that text: `parse_regex_uncached` now
installs it in a thread-local `TopLevelSourceScope` for the duration of a
top-level parse, and sub-pattern parses (groups, lookaround bodies, alternation
branches all re-enter the same function) *inherit* it rather than overwriting
it — Raku's `<~~>` recurses into the whole enclosing regex or rule body, not
into the bracket it happens to sit in. Inside a grammar the enclosing text is
the token's own body, so `<~~>` recurses into that token, which is what Rakudo
does:

```raku
grammar G { token TOP { '[' [ <-[\[\]]>* <~~> <-[\[\]]>* ]* ']' || '[' <-[\[\]]>* ']' } }
say G.parse('[a[b]c]');   # ｢[a[b]c]｣
```

At match time the atom re-parses that source through `parse_regex`, which is
memoized, so the recursion is a cache hit and a refcount bump rather than a
re-parse. The recursive invocation's captures are discarded (the `<$var>`
sub-match rule: the inner match gets its own `Match`, which must not leak into
the caller's `$/`).

Termination is guarded by a `RECURSE_SELF_STACK` of `(source, position)` pairs:
re-entering the *same* regex at the *same* position cannot consume anything, so
it fails instead of looping. That is what makes `/ <~~> a /` return `Nil`
rather than exhausting the stack — Rakudo itself dies mid-file on that input,
so mutsu is strictly better behaved there and the local test deliberately does
not assert it (the test file has to pass under `raku` too).

The scope guard is gated on the pattern containing `<~~` before it allocates,
so a regex that does not use the construct pays one substring search on the
parse path and nothing else.

`<~~N>` (recursing into a numbered capture) is still unimplemented; so is it in
Rakudo, which rejects it with "Sorry, ~~ regex assertion with a capture is not
yet implemented".

Pinned by `t/regex-engine-gaps.t`.
