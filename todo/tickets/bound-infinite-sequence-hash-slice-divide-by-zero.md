# Indexing a hash-bound infinite sequence via `%h{$key}[idx]` divides by zero

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/subscripts.rakudoc:462`).

## Repro

```raku
my @fib = 1,1, * + * … ∞;
my @lucas = 1,3, * + * … ∞;
my %sequences;
%sequences<f> := @fib;
%sequences<l> := @lucas;
for %sequences.keys -> $s {
    for ^10 -> $n {
        say %sequences{$s}[100+$n*10]/%sequences{$s}[101+$n*10];
    }
}
# OUTPUT: 0.6180339887498949 times 20.
```

- raku: prints `0.6180339887498949` 20 times (the golden-ratio convergence ratio, for both bound
  sequences × 10 indices each).
- mutsu (`target/debug/mutsu`): crashes on the very first division —
  ```
  Attempt to divide 0 by zero
    in block <unit> at ... line 8
  ```

## Analysis

`%sequences<f> := @fib` binds the hash value directly to the (lazy, infinite) `@fib` array via
`:=` (no copy). Reading `%sequences{$s}[100+$n*10]` (an element deep into the infinite sequence,
reached indirectly through a hash-bound alias) returns `0` in mutsu instead of forcing the
sequence to reify up to that index — so the division sees `0/0`-like results and dies. Indexing
the array directly (`@fib[100]`) likely already reifies correctly (per the lazy-list work noted
in `docs/doc-diff-backlog.md`'s Deferred section); the gap here is specific to reaching the same
lazy array *through* a `:=`-bound hash value subscript chain (`%h{key}[idx]`), which appears to
not trigger the same on-demand reification path.

## Affected files (starting point)

- `src/vm/vm_var_ops.rs` — hash-then-array chained subscript read path, specifically when the
  hash value is a `:=`-bound alias to a lazy/infinite array
- Compare against `@fib[100]` (direct array indexing, expected to already work per the lazy-list
  cluster fixes) to isolate what's different about the hash-indirection path

## Suggested next step

Minimize further: does `my @b := @fib; say @b[100];` (bind through a scalar-ish alias, no hash)
reproduce the same zero? That would narrow it to the `:=`-bind-then-reify gap rather than
something hash-subscript-specific.
