# A Pair built over `my $ = ...` holds a writable container

```raku
my $p = (a => my $ = 42);
$p.value = 5;                               # was: Cannot modify an immutable Any
for $p.kv -> $k, $v is rw { $v += 100 }
say $p.value;                               # 105
```

`k => my $ = v` made the Pair's value a read-only item instead of the anonymous
scalar's container, so assigning to `.value`, or through an `is rw` loop alias
over `.value`/`.kv`, failed or wrote a copy that only reached the frame holding
the Pair through a call's return merge. The Pair now holds the container, as
in rakudo, and an anonymous `my $ = Nil` holds its default `Any` (ADR-12529
§3, slice 27).
