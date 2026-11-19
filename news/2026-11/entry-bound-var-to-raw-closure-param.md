# A variable bound to a missing element passes its location to a raw closure parameter

```raku
my %h; my $c := %h<a>;
(-> $h is raw { $h = 5 })($c);
say %h;   # was {}, now {a => 5} as in Rakudo
```

`my $c := %h<a>` on a missing key leaves `$c` holding a deferred entry token:
the element is created by the first write through `$c`. An argument is read as
the element's current value, so a closure's `is raw` / `is rw` / `\x` parameter
was bound to `Any` rather than to the location, and the rw writeback after the
call skipped the token-holding variable. A closure called through a value now
receives the token itself for such a parameter, so a write or a bind below it
reaches the hash, and the writeback of a call that wrote the parameter
creates the element through the token (#18186). A readonly parameter, and a
call that never writes, still create nothing.
