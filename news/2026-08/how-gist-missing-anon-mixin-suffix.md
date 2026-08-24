# Preserve anonymous implementation roles on collection metaobjects

Mutsu now models the anonymous implementation-role compositions carried by
Rakudo's mutable built-in collection metaobjects.  Hash, Set, Bag, and Mix HOWs
have one anonymous mixin layer, while Array HOWs have two.

This fixes introspection such as:

```raku
say (%).HOW.^name
```

which now reports `Perl6::Metamodel::ClassHOW+{<anon>}`.  The suffix is not a
display-only adjustment: the HOW is represented with the existing runtime mixin
model, so `WHAT`, type naming, identity, and method dispatch consistently see
the composed metaobject.
