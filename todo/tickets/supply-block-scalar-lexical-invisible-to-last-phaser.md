# A supply block's scalar lexical, written from a `whenever` body, is stale in `LAST`

A `supply { ... }` block's own `my $x` is the natural accumulator for a
`whenever` that folds a stream and emits the result from its `LAST` phaser. The
`whenever` body's writes to it are lost: `LAST` reads the value the variable had
when the block started.

```raku
my $p = Supplier.new;
my $s = $p.Supply;
start { sleep 0.2; $p.emit(5); $p.done }
say await Promise(supply {
    my $sum = 0;
    whenever $s -> $v {
        $sum += $v;           # runs, $sum is 5 inside the body
        LAST emit $sum;       # sees 0
    }
});
```

raku says 5, mutsu says 0. The `whenever` body does run and does see its own
write (printing `$sum` inside the body gives 5); only the `LAST` phaser — and
anything else reading the variable after the body returns — sees the stale
value.

A *container* mutation is unaffected, which is why this stayed hidden:
`my $joined = Buf.new; ... $joined.append($blob); LAST emit $joined` works,
since nothing rebinds the scalar. Only a scalar assignment/`+=` is lost. That is
the signature of the writeback path: the `whenever` body's captured-outer scalar
writes are not drained back into the enclosing supply-block frame before the
`LAST` callbacks run.

Related, and probably the same machinery:
[`supply-block-lexical-leaks-through-thread-lane.md`](supply-block-lexical-leaks-through-thread-lane.md),
[`whenever-owned-lexical-outlives-the-react-block.md`](whenever-owned-lexical-outlives-the-react-block.md),
and `todo/deep/supply-block-lexicals-alias-the-caller.md`. #5704 made a supply
body's `my` private to the block (the right direction); this is the remaining
half — the private copy has to be written back before the phasers observe it.

Found while fixing the empty Cro response body
(`news/2026-08/preserving-supply-keeps-its-terminal.md`); Cro itself uses the
`Buf.append` shape, so it is not blocked by this.
