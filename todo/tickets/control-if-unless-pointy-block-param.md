# `if`/`unless`/`while` block body rejects a pointy-block parameter

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
388).

## Repro

```
$_ = 1;
unless 0 -> $_ { $_.say };
```

- raku: `0` (the pointy-block parameter `$_` shadows the topic, binding to the condition's
  value)
- mutsu: `===SORRY!=== Error while compiling ... Missing block ... at -e:1`

## Root cause guess

The parser for `if`/`unless`/`while` (and likely `until`) block bodies only accepts a plain
`{ ... }` block; it doesn't accept a `-> $param { ... }` signature form on the conditional
block, even though Raku allows a pointy block there (the condition's value is passed as the
block's argument, in addition to being available as `.so`/topicalized in some contexts).

## Affected files (starting point)

- `src/parser/` — wherever `if`/`unless`/`while` statement parsing consumes the block body
  (search for the "Missing block" error message to find the exact call site)

## Suggested next step

Grep the parser for the `if`/`unless` block-parsing function and check whether it calls the
same block-parsing helper used by `for`/`given` (which already accept pointy-block params), or
a separate one that needs the same extension.
