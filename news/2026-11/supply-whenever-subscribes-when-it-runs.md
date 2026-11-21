# A supply block's `whenever` subscribes when it runs

mutsu opens the sources of a `supply` block's `whenever`s only after the
block body has returned. A live `Supplier` that the body `whenever`ed and
then emitted into -- directly, or from a tap callback the body triggered --
had no subscriber yet, and the values were lost (#18524):

```raku
my $s = supply {
    my $forward = Supplier.new;
    whenever $forward.Supply -> $v { emit "fwd:$v" }
    $forward.emit(1);
    emit "body";
};
$s.tap(&say);   # rakudo: body, fwd:1 — mutsu printed only body
```

The `whenever` now starts collecting its live source's emissions the moment
it runs; when the block opens the source after the body, the collection is
swapped for the real tap under the emit gate (nothing falls between them or
arrives twice) and the collected values reach the `whenever` body, after the
body's own emits, as in rakudo. Red's `ResultSeq.Supply` forwards insert
events made during its snapshot this way: Red 0.2.16
`t/190-resultseq-supply` tests 33 and 34 now pass.
