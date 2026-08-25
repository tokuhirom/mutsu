# An embedded code block inside a quantified group doesn't persist its side effect on an outer `:my` variable

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1587).

## Repro

```
my $paragraph = "line\nline2\nline3";
$paragraph ~~ rx| :my $counter = 0; ( \V* { ++$counter } ) *%% \n |;
say "Matched $counter lines";
```

- raku: `Matched 3 lines` (the embedded `{ ++$counter }` block runs once per quantifier
  repetition, incrementing the `:my`-declared counter each time)
- mutsu: `Matched  lines` (`$counter` is empty/undefined — the increments inside the quantified
  group never reached the outer `:my $counter` binding)

## Root cause guess

Combines two already-suspect mechanisms (both also seen failing elsewhere in this batch): a
regex-embedded `:my $var = ...;` declarator, and an embedded `{...}` code block executed once
per quantifier iteration (`*%%`). The counter's mutation inside the code block presumably isn't
writing back to the same binding the `:my` declarator created — possibly because each quantifier
iteration re-evaluates the embedded block in a fresh scope instead of sharing the enclosing
regex's `:my`-declared lexical.

## Affected files (starting point)

- `src/runtime/regex.rs` — embedded code-block execution inside a quantified group, `:my`
  variable scoping across quantifier iterations

## Suggested next step

First isolate whether a *simpler* case works: `:my $counter = 0; ( a { ++$counter } )*` (no `%%`
separator, plain `*` quantifier) — if that already fails, the bug is about embedded-code-in-
quantifier scoping generally, not the `*%%` separator form specifically.
