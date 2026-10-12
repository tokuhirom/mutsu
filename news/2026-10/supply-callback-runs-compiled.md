# A Supply callback runs as a compiled block

`$supply.tap(-> $x { ... })` and the callbacks of derived supplies (`.map`,
`.grep` and the rest) went through the carrier path, which recompiles the block
from its syntax tree on each call. A closure made inside that recompiled body
lost the knowledge that its captured `$x` is never written again. So when a
later invocation of the same callback called the closure, the closure read the
later invocation's `$x` instead of its own (#12872):

```raku
my @cl;
my $s = Supplier.new;
$s.Supply.tap(-> $x { if $x eq 'stop' { .() for @cl } else { @cl.push: { say $x } } });
$s.emit('a'); $s.emit('stop');   # now prints "a", as rakudo does
```

A compiled block is now called through the VM's closure dispatch, the way a
`.map` callback is. `supply` and `whenever` bodies still take the carrier.
`whenever` callbacks are built from the syntax tree with no compiled chunk of
their own, so the same shape inside a `whenever` is still open in #12872.
