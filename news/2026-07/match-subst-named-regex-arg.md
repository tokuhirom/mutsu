# `.match` / `.subst` accept a named regex passed as a value (`&Named`)

A named regex or token passed as a *value* — `&Named`, or a hash/array slot
holding one — now works as the pattern argument of `.match` and `.subst`:

```raku
my regex Word { \w+ }
"foo bar baz".match(&Word, :g).elems;      # 3
"foo bar baz".subst(&Word, 'X', :g);       # "X X X"

my %search = word => &Word;
"ab cd".match(%search<word>, :g);          # matches "ab", "cd"
```

mutsu represents `&Named` as a `Routine` value (flagged `is_regex`), and both
`.match` and `.subst` previously accepted only a `Regex` value (`rx/.../`) or a
`Str`. A `Routine` fell through to the literal-string branch and matched its
stringified `sub Named (...) {...}` form, so it never matched — `.match` returned
`Nil` and `.subst` left the string unchanged. (Smart-match, `$str ~~ &Named`,
already worked.)

Both dispatchers now detect a regex `Routine` and extract its source via
`extract_token_regex_pattern` — the same path smart-match uses — then match with
that source, so the returned `Match` carries the named regex's own captures at
top level (`$m<k>`, not `$m<Named><k>`).

This is a step toward the `Text::CodeProcessing` distribution (T-057), which
stores its per-format search regex as `&MarkdownSearch` in a hash and drives
`.match(:g)` / `.subst` with it. A separate regex-engine bug remains for that
distribution (a frugal `.*?` combined with a `<?after>` look-behind and a
capture back-reference does not match minimally under `:g`), tracked in the
dist-ticket ledger. Pin: `t/match-subst-named-regex-arg.t`.
