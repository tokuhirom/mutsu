# `eager gather` no longer resets outer locals written in the same loop

```raku
sub f { my $t = 0; for ^3 { my @a = eager gather { take 1 }; $t += 1 }; $t }
say f();   # mutsu was 1, rakudo 3
```

(issue #9165) After forcing a gather, the `OpCode::Eager` arm copied every
env entry for the frame's locals back over the frame's slots, so that a
gather body's write to an outer variable (`$was-lazy = 0`) became visible to
the caller. But a slot-only local's env entry can be stale: the loop body's
`$t += 1` went to the slot, the env still held `0`, and the next `eager`
copied that `0` back over the slot. Without `eager` the output was already
right.

The arm now snapshots the env values of the frame's locals before the force
and copies back only the names whose env value the force actually changed,
the same snapshot-and-diff the `andthen` user-`.defined` path uses. A write
inside the gather body still reaches the outer local, and slot writes made
around it are kept.

The pin, `t/control/eager-gather-outer-local-writes.t`, writes its TAP by hand
instead of loading `Test`: a `use` at the top of the file changes how the
frame's locals are mirrored into env and hides the bug.
