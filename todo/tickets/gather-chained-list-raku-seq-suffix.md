# `(gather {...}).list.raku` keeps a spurious `.Seq` suffix when chained directly

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/List.rakudoc:219`,
minimized further during triage).

## Root cause

`.list` on a `Seq` produced by a `gather` block does not fully coerce to a `List` when
called **directly on the gather expression** (no intermediate variable). When the gather
result is first bound/assigned to a variable and `.list` is called on that variable, the
coercion works correctly. `.raku` on the result reveals the difference — the
direct-chain form still reports itself as a `Seq`:

```raku
put (gather { }).list.raku;                 # raku: ()        mutsu: ().Seq
my $s := gather { }; put $s.list.raku;       # raku: ()        mutsu: ()      (matches)
my $s = gather { }; put $s.list.raku;        # raku: ()        mutsu: ()      (matches)

put (gather { take 1; take 2 }).list.raku;   # raku: (1, 2)    mutsu: (1, 2).Seq
```

## Minimal repro

```raku
put (gather { take 1; take 2 }).list.raku;
```

- `raku`: `(1, 2)`
- `mutsu` (`target/debug/mutsu`): `(1, 2).Seq`

## Affected files (starting point)

The `.list` method dispatch on a `Seq`/gather-produced value — likely a compiler-level
difference in how a method call chained directly onto a `gather {...}` expression result
is compiled vs. a method call on a variable holding the same value (the receiver's
runtime representation/type tag isn't being updated to `List` in the direct-chain case).
Look at how `.list` coercion is implemented for Seq (`builtins/methods_0arg` or the
Seq→List coercion helper) and how the gather-expression compile path hands its result to
a chained postfix method call vs. a `my $x = ...` assignment.
