# Rebinding an `is rw` parameter no longer overwrites the caller's variable

A scalar `is rw` parameter on the full call path (any body containing a `for`
loop, an inner sub, a closure call, ...) is bound copy-in/copy-out: the body
works on its own slot and the slot's final value is written back on return.
A `$p := ...` rebind in such a body detaches the name from the caller's
variable, but the writeback still copied the rebound value back, so

```raku
sub a($p is rw) { for 1 { }; $p := $p<a>; 0 }
my $e = {}; a($e); say $e.raku;   # was Any, now ${}
```

clobbered the caller. A call now tracks the rw parameters its body can rebind
(`CompiledCode::rebound_slots`); the first rebind snapshots the slot's value,
and the writeback of the sub, method and pointy-block call paths uses that
snapshot. This unblocks `TOML::Thumb`'s `walk-key`, which descends a table by
rebinding its `$ptr is rw` parameter inside a `for` loop (#10361).
