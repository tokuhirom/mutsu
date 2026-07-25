# Proto-token LTM breaks an equal-length tie by declaration order

Rakudo's Longest-Token-Matching, when two `proto token` `:sym<>` candidates
match the *same* number of characters at a position, breaks the tie by
declaration order: the first-declared candidate wins. mutsu picked the wrong
candidate, so a globstar `**` in a `.gitignore`-style grammar

```raku
grammar G {
    token TOP { <pp>+ % '/' }
    proto token pp {*}
    token pp:sym<**> { <sym> }        # literal, declared first
    token pp:sym<m>  { <-[/]>+ }      # char-class fall-through
}
```

was dispatched to the `m` fall-through candidate instead of the dedicated `**`
candidate (`G.parse('d/**')` produced `M:d|M:**` instead of `M:d|GLOBSTAR`).

Two bugs combined to produce this:

1. The subrule resolver collected `:sym<>` variant candidates by walking a
   `HashMap` and then sorting the keys **alphabetically** (`sym_keys.sort()`) for
   determinism — so `pp:sym<**>` always sorted before `pp:sym<m>` regardless of
   source order, losing declaration order entirely.
2. The quantified-subrule LTM dedup in `regex_match_atom.rs` kept the **last**
   candidate on an equal-length tie (it popped the previous match and pushed the
   new one), rather than the first.

The fix records declaration order and honours it:

- `FunctionDef` gains a monotonic `decl_order`, stamped at `insert_token_def`
  time (grammar bodies register their tokens top-to-bottom, so registration
  order is declaration order). It rides along with the `Arc<FunctionDef>` stored
  in `token_defs`, so it survives every registry clone/snapshot.
- All five sym-key resolution sites sort by `sort_sym_keys_by_decl_order`
  (declaration order, alphabetical as a stable fallback) instead of
  alphabetically.
- The `regex_match_atom.rs` LTM dedup keeps the first-declared candidate on an
  equal-length tie.

Longest-token-still-wins on a genuine length difference, and the fall-through
candidate still wins only when the specific candidates fail to match.

Pinned by `t/proto-token-ltm-tiebreak.t` and the existing
`roast/S05-metasyntax/proto-token-ltm.t`. This takes the `File::Ignore`
distribution's `wildcard.rakutest` from 36/44 to 38/44 (the remaining `a/**/b`
mid-path globstar cases are a separate grammar dynamic-variable bug, recorded in
PLAN.md 8.20).
