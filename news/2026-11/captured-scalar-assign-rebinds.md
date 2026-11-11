# Assigning to a captured scalar no longer overwrites the container it held

A routine or closure that assigned to an outer `$x` holding an Array or a Hash
wrote the new contents *into* that container instead of rebinding `$x`:

```raku
my $e;
sub set-e($v) { $e = $v }
my %h = c => 1;
set-e(%h);
set-e({c => 9});
say %h;   # was {c => 9}; now {c => 1}, as in rakudo
```

The write reached the scalar's shared cell through
`cell_store_preserving_container_identity`, which copies a whole-container
reassignment into the existing backing store so an `@a`/`%h` keeps its
identity. That is right for an `@`/`%` variable, which *is* its container, and
wrong for a scalar, which only holds one. The in-place path now applies to `@`
and `%` names only.

Found while chasing EEL 0.3's fork state restore (#16888); the remaining part
of that failure, a closure's `%h := ...` lost after `%h` was passed to a `$`
parameter, is #17454.
