# A supply block's CLOSE phaser sees the block's earlier declarations

A `CLOSE { }` phaser in a `supply { }` block read a `my` variable that the block
declared before it as `Nil`:

```raku
supply {
    my $id = $ids.pull-one;
    CLOSE { $conn.print: "UNSUBSCRIBE $id" }   # $id was Nil
    whenever $incoming { ... }
}
```

The parser turns each CLOSE phaser into a registration call on the block's
emitter. It hoisted every registration to the head of its block, so a CLOSE
written after a loop is still registered before the loop runs. That hoisting
also moved the registration ahead of `my $id = ...`. The phaser's closure was
then built before the variable existed and captured it uninitialised.

A registration is now hoisted only as far as the last top-level declaration
of a variable its body reads. A CLOSE that reads no such variable still goes
to the block head, and the registrations keep their source order. A loop
after those declarations still sees the phaser registered ahead of it. The
`supply { my $stop = False; until $stop { ... } CLOSE { $stop = True } }`
shape keeps working.

The Stomp distribution surfaced this (#10832). `Stomp::Client.subscribe`
sends `UNSUBSCRIBE` with the subscription id from such a phaser, and that id
went out empty.
