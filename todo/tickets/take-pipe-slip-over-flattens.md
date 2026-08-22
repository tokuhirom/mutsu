# `take |($a, $b)` over-flattens like `.Slip`, instead of bundling into one `take`d item

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Slip.rakudoc:57`).

## Repro

```raku
my \l = gather for 1..10 -> $a, $b { take |($a, $b) }; say l.raku;
# raku:  ((1, 2), (3, 4), (5, 6), (7, 8), (9, 10)).Seq
# mutsu: (1, 2, 3, 4, 5, 6, 7, 8, 9, 10).Seq

my \m = gather for 1..10 -> $a, $b { take ($a, $b).Slip }; say m.raku;
# raku:  (1, 2, 3, 4, 5, 6, 7, 8, 9, 10).Seq
# mutsu: (1, 2, 3, 4, 5, 6, 7, 8, 9, 10).Seq   -- this one already matches
```

Minimal isolation:

```raku
my \l = gather { take |(1,2) }; say l.raku;
# raku:  ((1, 2),).Seq   -- take() got 2 positional args, bundled into one List item
# mutsu: (1, 2).Seq      -- treated as if take() got 2 separate items (Slip-like over-flatten)
```

## Root cause hypothesis

`|(...)` (the flattening prefix) in an argument-list position unpacks the list into multiple
*positional arguments to the call* — so `take |($a, $b)` is equivalent to `take($a, $b)`, i.e.
one call to `take` with 2 positional arguments. `take` with multiple positional arguments
bundles them into a single taken `List` item (matching how `take` treats its argument list in
general), which is a different operation from `.Slip`, which flattens *values already produced*
directly into the enclosing sequence as separate items. mutsu appears to treat both forms
identically — likely by resolving `|(...)` all the way down to a runtime Slip value that then
gets flattened by `take`/`gather`'s take-collection logic, rather than expanding `|(...)` at
the call-argument level before `take` ever sees a single combined value.

## Affected files (starting point)

- The `|` (flattening) prefix-operator's argument-expansion logic at call sites — confirm
  whether it currently produces a runtime `Slip` value passed as `take`'s single argument
  (wrong for this case) instead of expanding into N separate positional arguments to `take` at
  compile/call time (which `take`'s own multi-arg-bundling logic would then combine correctly).
- `take`'s multi-argument handling (`runtime/` gather/take implementation) — confirm it already
  bundles multiple explicit positional args into one List item (as the working `.Slip` case's
  contrast suggests it does for the direct-args case), so the fix is likely isolated to the `|`
  expansion, not `take` itself.
