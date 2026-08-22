# `s///` replacement string doesn't evaluate an embedded `{ ... }` code block, and over-escapes a literal `:`

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1823).

## Repro

```
$_ = '18:38';
s/(\d+)\:(\d+)/{$0 % 12}\:$1 {$0 < 12 ?? 'AM' !! 'PM'}/;
.say;
```

- raku: `6:38 PM`
- mutsu: `6\:38 PM`

Two distinct bugs visible here:
1. The two embedded `{ ... }` code blocks in the replacement string are supposed to be evaluated
   and their results interpolated (`{$0 % 12}` → `6`, `{$0 < 12 ?? 'AM' !! 'PM'}` → `PM`) —
   mutsu's `6` and `PM` DO appear correctly in the output, so code-block evaluation itself
   partially works.
2. The literal escaped colon `\:` in the replacement string is emitted **with the backslash
   still attached** (`6\:38` instead of `6:38`) — the replacement-string parser isn't stripping
   the backslash-escape for a literal `:` the way it should for an ordinary string/regex
   replacement text.

So the actual, narrower bug is #2 (`\:` renders literally); the `{ ... }` block evaluation
already works correctly in this example (re-read the outputs: `6` and `PM` did make it through).

## Root cause guess

The `s///` replacement-string lexer/interpolator likely only recognizes a small fixed set of
backslash escapes (`\n`, `\\`, `\/` maybe) and passes through `\:` unrecognized instead of
treating it as an escaped literal colon.

## Affected files (starting point)

- `src/vm/vm_string_regex_ops.rs` — substitution (`s///`) replacement-string escape handling
