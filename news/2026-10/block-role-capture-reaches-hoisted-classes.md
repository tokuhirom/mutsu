# A block role's captures reach classes composed before the block ran

A role declared in a block closes over the block's lexicals. An `our class`
that composes it later in the file is registered at program start by its
hoisted shell, before the block runs, so the methods and attribute defaults it
copied from the role had no capture of the block's variables:

```raku
{ my $h = 5; role R { method h { $h + 1 }; has $.a = $h } }
class E does R {}
say E.new.h;   # was 1, now 6
say E.new.a;   # was (Any) with a warning, now 5
```

When the role's registration runs inside the block, it now hands its method
captures (matched by name and candidate order) and its attribute-default
captures to every class that already composed it. A role attribute default
also captures its declaring scope's cells at the role's own registration, not
only when a composing class runs the role body (#13358).
