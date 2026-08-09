# Regex alternation `|` picks the first matching branch, not the LTM-longest declarative prefix

## Symptom

`Cro::HTTP` `t/http-router.rakutest` test 61 ("Two optional segments handled
correctly (none passed)"): requesting `/category/tree` returns
`category tree` instead of `category tree none-a none-b`.

The route block declares, in this order:

```raku
get -> 'category', Any $uuid { ... "category $uuid" ... }         # line 183
get -> 'category', 'tree', $a = 'none-a', $b = 'none-b' { ... }   # line 225
```

`Cro::HTTP::Router` compiles all routes into one `EVAL`'d regex of the shape
`[ <route1> | <route2> | ... ]`. For `/category/tree`, rakudo's `|` alternation
applies **LTM (longest token matching)**: the branch whose declarative prefix
matches the longest literal string wins, so `'category' '/' 'tree'` (two
literal segments) beats `'category' '/' <segment-capture>` regardless of
declaration order. mutsu's `|` appears to try branches in order and commit to
the first that matches, so the earlier `$uuid` route wins and the handler
output `category tree` (i.e. `$uuid = "tree"`) is exactly what the failing
test observes.

The two sibling cases pass only by accident of arity: `/category/tree/foo`
(3 segments) cannot match the 2-segment `$uuid` route at all, so order-based
first-match happens to pick the right branch.

## Why this is `deep`

Real LTM requires computing each alternative's declarative prefix (literals,
character classes, quantified atoms up to the first non-declarative construct)
and ordering candidate branches by longest-prefix-first at the alternation
point — a fundamental piece of Raku regex semantics (`|` vs `||`, which *is*
declaration-order first-match). mutsu currently treats `|` like `||`. Fixing
it touches the regex engine's alternation dispatch and likely needs a token
NFA/prefix-length analysis, not a local patch. Note `roast/S05-metasyntax/`
has dedicated LTM tests (`longest-alternative.t`) that would be the right
acceptance suite.

## Minimal repro (no Cro needed, verified 2026-08-09)

```raku
my $r = "/category/tree" ~~ / "/category/" (\w+) | "/category/tree" /;
say $0.defined ?? "capture-branch" !! "literal-branch";
# raku:  literal-branch  (the longer declarative prefix wins, despite being declared second)
# mutsu: capture-branch  (first declared branch that matches wins — the `||` semantics)
```

## Impact

- `http-router.rakutest` test 61 (one of only two remaining failures in that
  file as of `news/2026-08/parameter-named-names-plain-named.md`).
- Any grammar/regex relying on `|` LTM semantics — this is a broad
  correctness gap, likely affecting non-whitelisted S05 roast files.
