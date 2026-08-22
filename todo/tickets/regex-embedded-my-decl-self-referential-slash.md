# A regex-embedded `:my $c = $/;` declaration referencing the in-progress match (`$/`) fails to parse

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1595).

## Repro

```
"aba" ~~ / (a) b {} :my $c = $/; /;
say $c;
```

- raku: `｢ab｣` with `0 => ｢a｣` (assigns the in-progress match object, as captured so far, to
  `$c`)
- mutsu: `Runtime error: Regex not terminated.`

## Root cause guess

An embedded `:my $var = EXPR;` declarator inside a regex is parsed specially (it needs to stay
inside the regex's own mini-grammar rather than being treated as ordinary code), and the parser
presumably doesn't expect `$/` (the match-so-far pseudo-variable) to appear as the RHS of such a
declaration — possibly misinterpreting the `/` in `$/` as the regex's own closing delimiter,
which would explain "Regex not terminated" as a delimiter-matching failure.

## Affected files (starting point)

- `src/parser/` — regex-embedded `:my` declarator parsing, delimiter scanning for `/ ... /`
  regex literals

## Suggested next step

Check whether the parser's regex-delimiter scanner treats `$/` specially anywhere else inside a
regex body (e.g. inside a `{...}` code block it presumably already must, since `{ $/ }` embedded
code blocks work) — the `:my $c = $/;` declarator form may need the same delimiter-aware handling
extended to it.
