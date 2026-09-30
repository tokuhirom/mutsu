# The branches of a regex `&` / `&&` keep `:r` and `:s`

```raku
say ('xyz' ~~ / :r \w+ & xy /).gist;     # Nil      (was ｢xy｣)
say ('a b' ~~ / :s a b & a b /).gist;    # ｢a b｣    (was Nil)
say ('xyz' ~~ / \w+ & xy /).gist;        # ｢xy｣     (unchanged: no :r, so \w+ backtracks)
```

Each branch of a top-level `|` / `||` / `&` / `&&` is parsed as a pattern of its
own, so an inline adverb the enclosing regex consumed before the split is gone
unless it is put back on the branch. Alternation did that for `:i`, `:s` and
`:ratchet`. Conjunction did it for `:i` only, which had two visible effects:

- **`:r` (ratchet)** did not reach the branches, so `\w+` backtracked from `xyz`
  to `xy` to satisfy the other branch, where rakudo takes `xyz` possessively and
  the conjunction fails (#10353). The report looked like a parse problem, but
  the tree walk and the compiled engine agreed, because both received a pattern
  that had already lost the flag.
- **`:s` (sigspace)** was dropped the same way, which the issue did not mention:
  `/ :s a b & a b /` ran both branches without sigspace's whitespace matchers,
  so it did not match `'a b'` and did match `'ab'`.

The two splitters spelled the re-application out separately, which is how they
drifted. They now share `branch_with_inline_adverbs` in `regex_parse_core.rs`,
and the conjunction atom carries the ratchet flag the way a whole-pattern
alternation already did. A `token` and a regex declared with `:r` were affected
identically, and now agree with rakudo.

Pinned by `t/regex/syntax/regex-conjunction-inline-adverbs.t`, whose
expectations were taken from rakudo.
