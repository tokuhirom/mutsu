# A dying live `Supply.map` quits its supply instead of killing the emitter

On a Supplier-backed (live) supply, `map` and `do` run their callable while
`Supplier.emit` delivers the value. When that callable died, the exception
unwound straight out of `emit`, so the code feeding the supply died and the
tap's `quit` handler never ran:

```raku
my $s = Supplier.new;
$s.Supply.map(-> $ { die "boom" }).tap(-> $ {}, quit => { say "quit: ", .message });
$s.emit(1);   # mutsu died here with "boom"; rakudo prints "quit: boom"
```

Now the stage's own supply quits with the exception, as in rakudo: the tap's
`quit` handler (or a `whenever`'s `QUIT` phaser) receives it, `emit` returns
normally, and the stopped stage takes no further values (#18473). Red's live
ResultSeq supply relies on this to hand a failed row materialization to the
subscriber (`t/190-resultseq-supply`, "materialization error reaches the
subscriber").
