# Routines written into a named package's stash are installed and exported

`Foo::{'&bar'} = sub { ... }`, `Foo::<&bar> := &f`, and above all the
re-export idiom

```raku
my package EXPORT::DEFAULT { }
BEGIN for <&tags &bash> { EXPORT::DEFAULT::{$_} = ::($_) }
```

used to write into a throwaway copy of the stash, so the routine was neither
callable as `Foo::bar` nor exported. `Sparrow6::DSL` re-exports all of its
helper subs this way, which is why `Sparky-Job-Api` (a Sparrow6 user) died
with `Unknown function: tags`.

A `&` key (literal or computed at run time) on a named package's stash now
goes through the same keyed stash op `OUR::{...}` uses: the routine becomes
the package's symbol, and when the package is the loading module's
`EXPORT::<tag>` stash it is recorded as one of that module's exports. Other
keys keep the generic index-assign.

Routines stored under a sigil-leading name (`&Foo::bar`, which is also how
`Foo::.BIND-KEY('&bar', ...)` and `our &Foo::bar = ...` store them) are now
listed by `Foo::.keys` too.

Pinned by `t/modules/import-export/export-stash-named-keyed-assign.t`.
Sparky-Job-Api's `t/00-load.t` passes under mutsu.
