# `.snip` with multiple positional predicates silently drops all but the first

Discovered via the doc-diff harness on `raku-doc/doc/Type/Any.rakudoc` (around line 1525).

## Repro

```
use v6.e.PREVIEW;
.say for (5, 13, 29).snip(* < 10, * < 20);
```

- raku: `(5)` / `(13)` / `(29)` (each matcher advances round-robin as its predicate stops
  matching)
- mutsu: `(5)` / `(13 29)` (only the first matcher `* < 10` is honored; the second is ignored)

## Root cause (pinned)

`src/runtime/methods_dispatch_match2.rs::dispatch_snip_method` (~line 799-802) does
`let matcher = args[0].clone();` — it only ever looks at the *first* positional argument and
passes it alone to `eval_snip`, discarding any further comma-separated matcher arguments.

`eval_snip` itself (`src/runtime/builtins_collection_mapgrep.rs:276`) is already correct: it
supports a *list* of matchers (array/Seq/Slip) with round-robin advance. The bug is purely that
`dispatch_snip_method` never assembles the separate positional args into that list before
calling `eval_snip`.

## Suggested fix

In `dispatch_snip_method`, collect all positional `args` into a single list/array Value (instead
of taking only `args[0]`) before calling `eval_snip`, mirroring however `eval_snip` already
expects a multi-matcher list to look when passed as one array literal.

## Suggested test

`t/snip-multiple-predicates.t` covering both the doc's 2-predicate example and a 3+ predicate
case, comparing against raku's documented round-robin advance semantics.
