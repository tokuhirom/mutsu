# A Hash made with `nqp::create` shares the storage bound into it

nqp code builds a mutable view of a storage another object already uses by
creating an empty `Hash` wrapper and installing the storage into it:

```raku
my $hash := nqp::create(Hash);
nqp::bindattr(nqp::decont($hash), Map, '$!storage', $storage);
```

In rakudo every holder of `$storage` then sees what is written through the
view. mutsu has no wrapper/storage split — a Hash is its node — so the generic
bind copied the storage into the wrapper's own node, and later writes through
the view never reached the other holders. EEL 0.3's `Event.EEL-hash` is such a
view of an event's captures, which its runner also holds as the frame's
capture hash: a positive assertion's capture written through the view vanished
from the frame, and the next capture written into the frame dropped it from
the event (the "replay preserves original ordinal" checks of #16888).

When the target is a scalar the routine declared as
`my $x := nqp::create(Hash)` and is still empty, the bind now rebinds the
scalar to a `Hash` view of the storage's node instead, so both name one store.
A target someone else holds (`fill(%h)` installing a fresh `nqp::hash`) keeps
the existing behaviour.
