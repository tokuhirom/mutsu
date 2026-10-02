# `need` populates the module's EXPORT stash

`need Mod` used to load the module with exports suppressed, so its `is export`
routines were never recorded and `Mod::EXPORT::DEFAULT.WHO` was empty. Rakudo
only withholds the *import* into the needing scope; the stash itself is
populated. mutsu now registers a needed module's exports too, so the common
re-export idiom works:

```raku
need Rx::Ops;
sub EXPORT { Map.new(Rx::Ops::EXPORT::DEFAULT.WHO.pairs) }
```

Two further fixes were needed for it. An export-stash entry for a routine that
was never imported resolved to a by-name routine reference, which a `sub EXPORT`
hook then installed under the very name it dispatched through, recursing until
the stack overflowed; the entry is now built from the module's own candidates
(the `Mod::EXPORT::ALL::name` aliases). And a `need` of a module that another
compunit had already loaded skipped the package-visibility grant a re-`use`
replays, so `Mod::EXPORT::DEFAULT` was "Could not find symbol" from the second
needer. (#10683)
