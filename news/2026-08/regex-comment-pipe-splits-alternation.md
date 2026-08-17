# A `|`/`&` character inside a regex comment no longer confuses top-level alternation/conjunction splitting

```raku
"bar" ~~ /
    #a comment mentioning the pipe `|` character
    | 'bar'
    | 'baz'
/;
say ~$/;   # bar
```

`split_top_level_alternation`/`split_top_level_conjunction`
(`src/runtime/regex_parse_ltm.rs`) scan a regex pattern character-by-character
to find top-level `|`/`||`/`&`/`&&` operators, tracking `()`/`[]`/`{}`/`<>`
nesting depth and `'`/`"` quote state. They used to not recognize `#...`
(line) or `` #`[...] `` (embedded) regex comments, so a literal `|`/`&`
character inside a comment was treated as a real split point — the comment
above got split into two spurious branches, one of which started mid-comment
and failed to tokenize.

This was already fixed (commit `1763cbfa2`, "ignore comments when splitting
regex operators"): both split functions now call a shared
`consume_regex_comment` cursor helper (`src/runtime/regex_parse.rs`) that
recognizes both comment forms, consolidating logic that used to be
duplicated three ways across the tokenizer, `interpolate_regex_scalars`, and
the two split functions. Coverage lives in
`t/regex-alternation-leading-comment-branch.t` ("operators inside regex
comments are inert" cases). This ticket had not been retired from
`todo/tickets/` when the fix landed — this entry closes that gap with no
further code change needed; the original repro (and an embedded-comment
variant) were re-verified against `raku` before retiring it.
