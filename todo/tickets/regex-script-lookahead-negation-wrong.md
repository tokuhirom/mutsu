# `<!:Script<Name>>` Unicode-property negated lookahead doesn't stop the preceding quantifier

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1331).

## Repro

```
say '333' ~~ m/^^ \d+ <!:Script<Tamil>> /;
```

- raku: `｢33｣` (the greedy `\d+` backtracks by one so the negated lookahead can succeed at that
  position — a subtle interaction between quantifier backtracking and a Unicode-property
  negated assertion)
- mutsu: `｢333｣` — matches the full string, meaning the `<!:Script<Tamil>>` assertion isn't
  actually constraining where `\d+` stops

The other 3 examples on the same lines (`<?alnum>`, `<?:Nd>`, `<!:L>`) all already match raku
correctly — only the `:Script<...>`-parameterized property negation misbehaves.

## Root cause guess

`<!:Script<Tamil>>` is a negated Unicode-property assertion parameterized with a specific script
name. The plain `<!:L>` (general category negation) already works, so the bug is likely
specific to the parameterized `:Script<Name>` form not being evaluated at all (always
succeeding trivially, so the assertion never actually blocks backtracking).

## Affected files (starting point)

- `src/runtime/regex.rs` / `src/runtime/regex_parse.rs` — Unicode property assertion handling,
  specifically the `:Script<...>` parameterized form vs. plain category assertions
