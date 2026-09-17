# Typed containers keep values assigned in `BEGIN`

A bare typed array or hash declaration could erase a value assigned to the same
lexical container by a top-level `BEGIN` phaser:

```raku
my Int %counts;
BEGIN %counts = (ten => 10);
say %counts<ten>; # 10
```

mutsu now keeps the `BEGIN`-created container. The declaration's type
constraint is still registered at block entry, and later assignments continue
to enforce it. Closes #8448.
