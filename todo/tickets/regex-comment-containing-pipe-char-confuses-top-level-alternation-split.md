# A `|` character inside a `#` regex comment is misread as a real alternation separator

`Interpreter::split_top_level_alternation` (`src/runtime/regex_parse_ltm.rs`) and its
sibling `split_top_level_conjunction` scan a regex pattern character-by-character to find
top-level `|`/`||`/`&`/`&&` operators, tracking `()`/`[]`/`{}`/`<>` nesting depth and
`'`/`"` quote state — but they do NOT recognize `#...` (line) or `` #`[...] `` (embedded)
regex comments. A literal `|` (or `&`) character that happens to appear inside a regex
comment is therefore treated as a real top-level alternation/conjunction split point.

## Repro

```raku
"bar" ~~ /
    #a comment mentioning the pipe `|` character
    | 'bar'
    | 'baz'
/;
say ~$/;
```

mutsu: `Runtime error: Unrecognized regex metacharacter `` ` `` (must be quoted to match
literally)` — the comment gets split into two spurious "branches" at its internal `|`,
one of which starts mid-comment with a bare `` ` ``, which then fails to tokenize as a
regex atom.

Expected (raku): `bar` — the comment is inert; only the two real `| 'bar'` / `| 'baz'`
branches (plus the leading whitespace/comment-only branch, elided by
`regex_branch_is_blank`, see ADR-0022 Slice 3 follow-up fix) participate.

## Root cause

Neither `split_top_level_alternation` nor `split_top_level_conjunction` skips over
`#...`/`` #`[...] `` comment spans before scanning for `|`/`||`/`&`/`&&`. The main
tokenizer (`regex_parse_core.rs`'s token loop) and `interpolate_regex_scalars` both
correctly recognize and skip these comment forms; the two `split_top_level_*` functions
do not share that logic.

## Why this is separate from the ADR-0022 Slice 3 fix

Discovered while writing a regression test for the ADR-0022 Slice 3 leading-empty-branch
bug (`t/regex-alternation-leading-comment-branch.t`) — an early draft of that test
accidentally put a `|` inside the branch's own comment text and hit this different,
pre-existing bug instead. It affects ordinary (non-declarative-prefix-ranking) alternation
splitting and predates ADR-0022 — confirmed it reproduces identically against `main`.

## Fix sketch

Give `split_top_level_alternation`/`split_top_level_conjunction` the same `#`-comment
skip logic already duplicated in the tokenizer and `interpolate_regex_scalars` (three
copies of essentially the same scan now) — ideally factor all three into one shared
comment-skipping cursor helper instead of a fourth copy-paste.

## Affected files

- `src/runtime/regex_parse_ltm.rs` (`split_top_level_alternation`,
  `split_top_level_conjunction` presumably lives nearby or in `regex_parse_core.rs`)
- `src/runtime/regex_parse_core.rs` (duplicate comment-skip logic to consolidate against)
- `src/runtime/regex_parse_modifier.rs` (`interpolate_regex_scalars`'s duplicate)
