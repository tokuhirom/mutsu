# A chained colon-pair adverb group before a positional argument misparses as a list

Found while writing a regression test for `chdir`'s adverb handling (see
`news/2026-08/chdir-adverbs-parsed-as-the-path.md`). It is unrelated to
`chdir`/`indir` specifically — it reproduces for any listop call, so it is
filed as its own ticket rather than folded into that fix.

## Repro

```
$ target/debug/mutsu --dump-ast -e 'foo :!d:r, "x", "y";'
```

parses as a top-level `ArrayLiteral` containing two elements:

1. `Call { name: "foo", args: [ :!d, :r ] }` — the call with only the two
   chained adverbs as its arguments
2. the string literals `"x"` and `"y"` as further list elements

instead of the correct single `Call { name: "foo", args: [ :!d, :r, "x", "y" ] }`.

Confirmed this only triggers when the adverbs are *chained* with no comma
between them (`:!d:r,`). The comma-separated form works correctly:

```
$ target/debug/mutsu --dump-ast -e 'foo :!d, :r, "x", "y";'
```

parses as a single `Call` with all four arguments in order, as expected.

A single adverb (chained or not, since there's nothing to chain) is also
unaffected: `chdir :!d, "x"` parses correctly as a two-argument call.

## Impact

Any listop-style call (`name arg1, arg2, ...` with no parens) that mixes a
chained multi-adverb group (two or more colon-pairs written back-to-back,
e.g. `:!d:r`) with at least one positional argument after it silently
produces the wrong AST — a list expression instead of a single call — with
no parse error. This is a silent semantic divergence, not a crash, which
makes it easy to miss: a script using this exact idiom (uncommon, but valid
Raku) would run to completion but produce a wrong result. `roast/S32-io/chdir.t`
does not exercise this form (it always uses one adverb per test call), so
it wasn't previously caught by the roast suite.

## Root cause (not yet fully investigated)

Not yet root-caused at the parser level. Hypothesis: whatever precedence
rule finishes consuming a bareword-call's comma-separated argument list
after a chained colon-pair group returns early — treating the *first*
comma after a colon-pair chain of length >= 2 as the top-level list-literal
comma operator, rather than as a separator continuing the same call's
argument list. A single colon-pair (chain length 1) does not trigger this,
suggesting the bug is specifically in how the parser represents/consumes a
multi-adverb `:a:b` group before deciding whether a trailing comma belongs
to the call or to an enclosing list.

## Affected files

Likely `src/parser/` — wherever bareword/listop call argument lists are
parsed and colon-pair adverb chains are recognized. Not investigated beyond
the AST-dump repro above.

## Workaround

Use the comma-separated adverb form (`chdir :!d, :r, $path` instead of
`chdir :!d:r, $path`) — this parses correctly and is accepted by rakudo too.
