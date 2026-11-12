# A BEGIN in a routine may read a `BEGIN my` unit lexical

```raku
BEGIN my $u = 1;
sub f { BEGIN say $u }
```

rakudo prints `1` at compile time, whether or not `f` is ever called. mutsu
printed nothing. A BEGIN nested in a routine runs at compile time by being
moved into the unit's prologue (ADR-0134). That move only reads names the
unit declares, and the list of those names left out declarations made by a
blockless phaser such as `BEGIN my $u = 1`. So the BEGIN stayed inside `f`'s
body and ran only if `f` was called. That list now includes those
declarations (#17567).
