# A role method calls its own role body's `sub` over the class's

When a class composing a role declared a `sub` with the same name as one in
the role body, every method called the class's sub. In Rakudo each method
resolves a bare routine call in the scope it was written in:

```raku
role R { my $count = 10; method m { f() }; sub f { ++$count } }
class K does R { sub f { "class f" }; method k { f() } }
say K.new.k; say K.new.m; say K.new.m;   # class f / 11 / 12
```

mutsu printed `class f` three times. The role body's subs are now declared,
for such a class, in a composition scope of their own (`K::&role:R`). It shares
the class's cells for the role body's lexicals, so `$count` still counts per
composing class. The role's methods composed into `K` run in that scope and
reach the role's subs first, then `K`'s package. The class's own methods keep
calling the class's subs.
