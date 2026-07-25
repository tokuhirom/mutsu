# Grammar actions run after every inline `{ … }` block, so a per-match `:my $*FINAL` reads the last segment's value

Found 2026-07-25 in File::Ignore. Moved out of PLAN.md §8.20 when discovered
findings became per-file `todo/` entries.

*(The original PLAN §8.20 — proto-token LTM equal-length tie-break — was FIXED
2026-07-25: the resolver sorted `:sym<>` candidates alphabetically instead of by
declaration order, and the quantified-subrule LTM dedup kept the LAST candidate
on a tie. Fix: stamp `FunctionDef.decl_order` at `insert_token_def`, sort
sym-variant keys by it (`sort_sym_keys_by_decl_order`), and keep the
first-declared candidate on an equal-length tie in `regex_match_atom.rs`. Pin:
`t/proto-token-ltm-tiebreak.t`. That took `File::Ignore` `wildcard.rakutest`
36/44 → 38/44. The remaining 6 (`a/**/b` mid-path globstar) are THIS separate
bug.)*

`File::Ignore`'s grammar declares a per-match dynamic variable and sets it inside
a lookahead, then reads it in the action method to decide whether a path segment
is the final one:

```raku
token path-part:sym<matcher> {
    :my $*FINAL;
    <matcher>+ {}
    [<?before '/'? $> { $*FINAL = True }]?   # sets $*FINAL only for the LAST segment
}
```

## Repro

```raku
grammar G {
    token TOP { <part>+ % '/' }
    token part {
        :my $*FINAL;
        \w+ {}
        [<?before '/'? $> { $*FINAL = True }]?
    }
}
class A {
    method TOP($/) { make $<part>.map(*.ast).join('|') }
    method part($/) { make ($*FINAL ?? "FIN" !! "mid") ~ ":$/" }
}
say G.parse('a/b/c', :actions(A)).ast;
# raku:  mid:a|mid:b|FIN:c      mutsu: FIN:a|FIN:b|FIN:c
```

## Root cause

Each `part` match should get its OWN `$*FINAL` binding (`:my $*FINAL` inside the
token), True only for the segment whose `<?before '/'? $>` lookahead succeeds
(the last). In mutsu every segment's action reads `FIN` — the value set during
the LAST segment's match leaks back to the earlier segments' action methods.

Confirmed 2026-07-25 (via HTTP::Header): mutsu builds the whole match tree first
and only then walks it, whereas raku runs an action method the moment its subrule
reduces. More precisely, on a *successful* parse mutsu makes two separate
bottom-up passes over the finished tree — `reduce_regex_captures_made` runs every
node's inline `{ … }` code blocks, and only then does `invoke_grammar_actions`
run every node's action. So by the time `part`'s action for segment `a` runs,
segment `c`'s `{ $*FINAL = True }` has already executed.

The **failed-parse half of this was fixed 2026-07-25**: when the overall parse
FAILS, mutsu used to run no actions at all. The matcher now logs every
named-subrule reduce (`REDUCED_SUBRULES` in `regex/regex_helpers.rs`), `.parse`
keeps the longest partial match instead of discarding it, and the failure path
dispatches that partial tree plus the reduces that fell outside it. That took
HTTP::UserAgent's upstream suite 23/27 → 25/27 (`t/010-headers`,
`t/050-response`). Pin: `t/grammar-actions-on-failed-parse.t`. **What is left is
only the ORDERING half — the `$*FINAL` repro above.**

## Affected files

- `src/runtime/methods_grammar.rs` — `invoke_grammar_actions`, called after the
  match returns.
- `reduce_regex_captures_made` — the separate pass that runs inline `{ … }`
  blocks.
- `src/runtime/regex/regex_helpers.rs` — `REDUCED_SUBRULES`.

## Why it is large

The fix does **not** need actions moved into the matcher: interleaving the two
passes into a single bottom-up walk (per node: code blocks, then that node's
action) reproduces raku's order — `mid:a|mid:b|FIN:c` — without touching the
regex engine. The work is that `reduce_regex_captures_made` walks
`RegexCaptures` while `invoke_grammar_actions` walks the built Match `Value`, so
the merged walk has to build each node's Match object as it descends, and mark
nodes whose action already ran so the outer pass cannot double-dispatch (the
hazard `dispatch_silent_action_caps` already documents).

## Impact

The 6 remaining `File::Ignore` `wildcard.rakutest` `a/**/b` failures. The
mid-path globstar's compiled regex is correct (a direct
`/^ 'a' '/' [ <-[/]>+ [ '/' | $ ] ]* 'b' <?before "/" | $> /` matches in mutsu);
the module builds a *wrong* pattern string (`'a'` missing its trailing ` '/'`)
because the `a` segment's action wrongly sees `$*FINAL = True`.
