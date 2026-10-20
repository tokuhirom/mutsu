# A role body's `sub` keeps its composing class's state

Rakudo runs a role body once per composing class, and a `sub` the body
declares closes over that run's lexicals:

```raku
role R { my $count = 10; method m { f() }; sub f { ++$count } }
class K does R {}
class L does R {}
say K.new.m; say K.new.m; say L.new.m;   # 11 12 11
```

mutsu kept the body's lexicals as statics of the composing class, where the
composed methods read and write them. The body's `sub`s, however, were
registered once in the role's package and closed over the body run's plain
values. So `++$count` started from a fresh `10` on every call and wrote to a
copy nobody saw (`11 11 11`).

After a composition stores the body's lexicals as the class's statics, each
plain body `sub` is now declared again in the composing class's package, with
the class's cells bound for those lexicals. A composed method, which runs in
that package, finds the class's own copy first.

Some subs are not re-declared:

- `multi` and exported subs, which are registered once when the role is
  declared.
- Subs with custom traits, which must not apply twice (#13645).
- A sub of the same name the class declares itself.

In that last case the role's methods still call the class's sub, as before.
Rakudo keeps the two scopes apart, and that remains to be done.
