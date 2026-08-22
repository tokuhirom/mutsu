# Colon-call syntax with zero arguments (`.method:` immediately followed by `;`) fails to parse

Discovered via the doc-diff harness on `raku-doc/doc/Language/objects.rakudoc` (around line 65).

## Repro

```
say 4.log:   ;
```

- raku: `1.386...` (parses `4.log:` with no following argument the same as plain `4.log`)
- mutsu: `===SORRY!=== Error while compiling ... Confused. expected statement`

`4.log: 2;` (colon-call with an actual argument) already parses fine in mutsu.

## Root cause guess

The colon-call argument parser (invoked after `.method:`) presumably assumes at least one
argument token follows the colon and fails/errors when it immediately hits a statement
terminator, instead of treating a colon-call with nothing after it as equivalent to a
zero-argument call.

## Affected files (starting point)

- `src/parser/expr/postfix/call_method.rs` (or wherever colon-call argument parsing lives —
  grep for the colon-call method-argument parsing function)

## Suggested next step

Find the exact parse function invoked right after consuming the `:` in a colon-call and check
what it does when the next token is a statement terminator (`;`) or closing delimiter instead of
an argument expression — it should back off to zero args rather than erroring.
