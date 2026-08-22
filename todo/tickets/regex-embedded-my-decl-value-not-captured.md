# A regex-embedded `:my $c = ~$0;` declaration captures an empty value instead of the current match text

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1602).

## Repro

```
"aba" ~~ / (a) {say "Check so far ", ~$/} b :my $c = ~$0; /;
say "Capture $c";
```

- raku: `Check so far a` then `Capture a`
- mutsu: `Check so far a` then `Capture` (empty — `$c` never got `~$0`'s value)

Notably, the plain embedded code block `{say "Check so far ", ~$/}` earlier in the same regex
*does* correctly see the in-progress `$/`/`$0` — the bug is specific to the `:my $var = EXPR;`
declarator form, not embedded-code access to captures in general.

## Root cause guess

Since a plain `{...}` code block can already read `$0`/`$/` correctly mid-regex, the bug is
localized to how the `:my $var = EXPR;` declarator evaluates and stores its RHS — it likely
either evaluates `EXPR` too early (before `$0` is bound) or fails to write the evaluated value
into the declared variable's binding that survives past the regex.

## Affected files (starting point)

- `src/runtime/regex.rs` / `src/parser/` — regex-embedded `:my` declarator evaluation and
  variable binding

## Suggested next step

Compare this to the working `{...}` code-block case: does `:my $c = EXPR` compile to a different
opcode/AST shape that evaluates `EXPR` at a different point in the match (e.g. at declaration
time before the capture group commits) rather than at the point the declarator statement is
reached during matching?
