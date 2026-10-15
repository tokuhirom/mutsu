# `$x does R` in a closure reaches the captured variable

```raku
role R { has $.tag = 'tagged' }
my $w = 42;
await start { $w does R; 1 };
say $w.tag;     # tagged (was: No such method 'tag' for invocant of type 'Int')
```

`does` on a variable rebinds the variable to the mixed-in value, as `=` does.
But the free-variable analysis did not count it as a write. So a closure that
mixed a role into a captured variable changed only its own copy. A sibling
closure, the caller of a `start` block, or code reading the variable after a
routine returned the closure all saw the plain value. Only a call made from
the declaring frame got the mixin back, through the routine-return merge.

The analysis now counts it as a write. The captured variable becomes a shared
cell, and the mixin is stored through it (ADR-12529 phase 3, slice 18).
