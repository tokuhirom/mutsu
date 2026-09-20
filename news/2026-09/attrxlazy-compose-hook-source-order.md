# AttrX::Lazy's own natural declaration order no longer breaks its `compose` hook

The real `AttrX::Lazy` distribution declares a lazy attribute before its private
builder method, exactly as its own README shows:

```raku
use AttrX::Lazy;

class Sample {
    has $.attribute is lazy;
    method !build_attribute() { 42 }
}
say Sample.new.attribute;
```

mutsu ran an attribute trait's mixed-in `compose` hook (the mechanism
`LazyAttributeContainerHOW.compose` uses to find lazy attributes and install
their accessor) inline, synchronously, as soon as the triggering `has`
statement's op ran during class-body registration. So a `compose` hook that
inspected `type.^private_method_table` for the builder always missed it when
the builder method came later in the class body -- Rakudo's own compose hook
sees every method declared anywhere in the class body by the time it runs,
regardless of source order, which we confirmed by probing `raku` directly
with a two-attribute repro.

The fix queues the owner class name instead of invoking `compose` right
away, and `run_class_body` drains that queue once every class-body
statement -- attributes AND methods -- has registered, re-reading the
owner's current `.HOW` from the registry at that point (rather than the
value captured when the mixin first happened) so a hook that runs after
further mixins still sees the fully-composed `HOW`. This mirrors the
existing `pending_class_compose` deferral already used for the
EXPORTHOW/DECLARE custom-HOW path.

Along the way, running the real vendored `AttrX::Lazy` module end to end
surfaced one more, unrelated gap: `.^roles_to_compose` (the
`Metamodel::ClassHOW` introspector for roles still queued for native
composition) was entirely unimplemented. Verified against `raku`: it
reports empty even for a class that already composed a role, since it
lists roles still queued rather than already-applied ones -- mutsu has no
such intermediate state to report, so it now always answers empty, which
matches every observable case.

With both fixed, the real `AttrX::Lazy` module's own example now runs
verbatim and prints `42`.

Fixes #8845.
