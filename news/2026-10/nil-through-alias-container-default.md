# Nil stored through an alias decays to the container's `is default`

Assigning `Nil` through an alias — an `is rw` parameter, a `for` alias
(`-> $v is rw`, `<->`, the topic), or a `:=` binding — now stores the default
of the container the alias writes into, as rakudo's container descriptor does:

```raku
my Int $x is default(42) = -1; for $x -> $v is rw { $v = Nil }; say $x;  # 42
my @a is default(3) = 1; for @a -> $v is rw { $v = Nil }; say @a;        # [3]
```

mutsu kept a scalar's `is default` only under the declared variable's name, so
a write arriving through any other name applied the alias's (absent) default
and stored `Nil`/`Any`. The default now rides on the shared `ContainerCell`
next to its `of`-type: scalar promotion sites copy the variable's default onto
the cell, array/hash element promotion copies the container's element
default, and the `SetLocal`, `SetGlobal` and `AssignExpr` stores decay a `Nil`
to the cell's default (#9831). The sigilless-parameter alias chain, which is
by name rather than by cell, is tracked separately in #11110.
