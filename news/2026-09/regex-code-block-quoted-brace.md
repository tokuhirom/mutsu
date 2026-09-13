# A `}` inside a string in a regex code block is code, not the closing brace

```
$ raku  -e 'grammar G { token TOP { { say "a}b".chars } \w+ } }; G.parse("zz")'
3
$ mutsu -e 'grammar G { token TOP { { say "a}b".chars } \w+ } }; G.parse("zz")'
(no output — the block never ran, and the token silently failed to match)
```

Three brace-depth scans in `src/runtime/regex_parse_core.rs` counted `{` and `}` without tracking
quotes, so a `}` inside a string literal ended the code block. `{ say "a}b".chars }` became the block
`{ say "a`, and `b".chars } \w+` was left to be parsed as pattern text. Nothing errored; the token
just stopped matching.

The sibling scanners already had the rule. `scan_code_assertion_body` (for `<{ … }>`) and
`find_matching_brace_end` (added by [#8317](https://github.com/tokuhirom/mutsu/issues/8317)) are both
quote-aware, and the latter's doc comment describes this exact failure. The three loops that were not
are now one shared `read_code_block_body`, which is the same rule in the spelling these callers need
— they hold a `char` iterator rather than a `&[char]` slice.

The three sites: the bare `{ … }` code block in a pattern, and the two `** { code }` repetition
quantifier spellings.

## The ticket's repro was not minimal, and its diagnosis pointed elsewhere

[#8336](https://github.com/tokuhirom/mutsu/issues/8336) reported this through a subrule argument —
`<inner("a}b")>` against a `token inner($x) { { say $x.chars } \w+ }` — and said explicitly that it
"reproduces from source with no parameter round trip involved", pointing at `scan_angle_construct`,
`split_regex_arg_list` and `parse_regex_lookup_target` as the likely culprits. All three of those are
quote-aware and none was at fault.

Narrowing the repro is what found the real site: dropping the code block from `inner` made it pass,
and so did dropping the `}` from the argument — so both were needed, which meant the argument was
not incidental to a *parse* failure but part of a *baked code text* one. The minimal case turned out
to need no subrule and no argument at all, just a code block with a `}` in a string. The `--dump-ast`
output also showed the token body arriving intact (`:ratchet { say "a}b".chars } \w+`), which ruled
out the parser before any of the named functions had to be read.

## Pin

`t/regex/regex-code-block-quoted-brace.t` — 8 tests, green under mutsu and under real Rakudo: the
bare code block in both quote flavours, the `<inner("a}b")>` shape the ticket reported, and both
`** { code }` quantifier spellings.

Two things the pin deliberately does not assert, both found by running it against rakudo:

- **An unmatched `{`** inside such a string (`"a{b"`) is a rakudo syntax error — "Two terms in a
  row" — so rakudo's own scan is not quote-aware for the opener either. There is no reference
  behaviour to match, so only the unmatched closer is covered.
- The cases use **distinct grammar names**. Two `grammar G` declarations in sibling bare blocks are a
  redeclaration in rakudo, which mutsu accepts; that is a separate divergence and not this file's
  subject.
