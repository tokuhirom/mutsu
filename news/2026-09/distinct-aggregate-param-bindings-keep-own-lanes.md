# Two live bindings of one `%`/`@` parameter no longer merge across a spawn

Two closures made by the same routine, each capturing its `%g` parameter bound
to a *different* caller hash, merged their stores once they spawned threads:

```raku
sub mk(%g) { -> $k { %g{$k} = 1; start { 1 } } }
my %one; my %two;
my &f = mk(%one); my &h = mk(%two);
f('a'); h('b'); f('c'); h('d');
say %one.keys.sort, ' ', %two.keys.sort;   # rakudo: (a c) (b d) — mutsu printed (a b c) (a b c d)
```

A spawn keeps a parameter-bound aggregate out of the name-keyed cross-thread
store, because a parameter is a fresh binding on every call. That check used a
`name -> container` map, so it remembered only the latest binding of `%g`. The
older closure's `%g` failed it, was seeded into the store under `%g`, and from
then on both closures' stores went through one shared copy.

The table (`ParamBoundAggregates`) now remembers every live binding per name.
It holds each container through a weak GC handle keyed by address. The weak
handle means it does not keep arguments alive. It also cannot confuse a reused
address with a recorded one, because an outstanding weak handle keeps the
node's allocation reserved. Dead entries are swept on an amortized schedule.

The same table now also answers `container_name_is_redeclared`, which the
container mutation routes consult before writing through the store. Before,
a captured `@g.push(...)` went to the name-keyed atomic store whether or not the
spawn had put `@g` there, so the array form of the example merged the same way.
Closes #10076.
