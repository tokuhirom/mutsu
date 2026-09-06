# A non-capturing group carrying a code block loses the chain under `** N..M % sep`

Found during ADR-0073's measurement sweep (2026-09-07) and confirmed to
**pre-date** it: the same wrong match is produced by the eager candidate
producer, so this is not a regression from the demand-driven change.

```raku
my $c = 0;
say "a,b,c" ~~ / [ \w+ { $c++ } ] ** 1..3 % ',' /;   # raku: ｢a,b,c｣   mutsu: ｢a｣
say $c;                                              # raku: 3        mutsu: 1
```

The block is what breaks it. Every neighbouring shape is right:

| shape | raku | mutsu |
| --- | --- | --- |
| `[ \w+ ] ** 1..3 % ','` (no block) | `a,b,c` | `a,b,c` |
| `( \w+ { B } ) ** 1..3 % ','` (capturing group) | `a,b,c` | `a,b,c` |
| `[ \w+ { B } ] +% ','` (`+` instead of `** 1..3`) | `a,b,c` | `a,b,c` |
| `[ \w+ { B } ] ** 1..3 % ','` | `a,b,c` | **`a`** |

So it needs all three of: a **non-capturing** group, a **code block** inside it,
and a **counted** (`** N..M`) separated quantifier. Swap any one of them and the
chain extends correctly.

Both the count and the match are pinned as `todo` in
`t/regex-lazy-candidate-enumeration.t` (F6, F6b), next to the `+%` and `*%%`
rows that do work, so a fix will show up as those two tests going from
unexpectedly-passing to passing.

## Where to look

`src/runtime/regex/regex_match_sep_lazy.rs` (`sep_extend_chain`) and its
collecting twin `src/runtime/regex/regex_match_sep.rs`
(`extend_separated_chain`) share the shape: extend the chain while
`walk.max.is_none_or(|m| atom_caps.len() < m)`, then report the node. The `max`
bound is the only thing the `** 1..3` case has that `+%` does not, and a
non-capturing group is the only atom shape whose delta carries no positional
slot — which suggests the interaction is between the `max`/`min` node filter and
what `count_capture_groups` reports for the atom (`atom_stride` is 0 for `[ ]`
and 1 for `( )`). Verify that guess before writing code; the sweep did not
confirm it.
