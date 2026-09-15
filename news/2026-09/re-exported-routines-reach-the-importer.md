# A re-exported routine reaches the importer

`JSON::Class` does not declare the attribute traits its users write. It re-exports
`JSON::Marshal`'s and `JSON::Unmarshal`'s, by binding them into its own export stash:

```raku
my package EXPORT::DEFAULT {
    OUR::{'&trait_mod:<is>'} := &trait_mod:<is>;
}
```

That is the whole re-export mechanism in Rakudo: the contents of a module's `EXPORT::<TAG>` stash
*are* its export list for that tag, however they got there — a `sub ... is export` declaration and a
runtime bind into the stash are two spellings of the same thing.

mutsu only ever built its export tables from the declarative half. A bind into `EXPORT::DEFAULT`
registered the routine under the `EXPORT::DEFAULT` package and stopped there: nothing attributed it
to the compunit being loaded, so `use JSON::Class` imported nothing at all from it and every
attribute trait reached through the re-export died as

```
Can't use unknown trait 'is' -> 'json-skip-null' in an attribute declaration.
```

`register_our_code_alias` now recognizes that the package it is binding into is the loading module's
export stash. In that case the alias family goes under the module's own name — the namespace
`import_module` reads an export back out of — and the routine is recorded as one of that module's
exports under the stash's tag. A plain (non-multi) routine re-exports the same way; before, only a
multi was considered at all, and only for the callable-through-the-stash case.

## The `&` sigil on a named parameter is a Callable constraint

Fixing the re-export exposed the second half of the same failure. `JSON::Marshal` declares its
`marshalled-by` trait as a pair of candidates:

```raku
multi sub trait_mod:<is> (Attribute $attr, :&marshalled-by!) is export { ... }
multi sub trait_mod:<is> (Attribute $attr, Str:D :$marshalled-by!) is export { ... }
```

mutsu enforced the `&` sigil's implied `Callable` constraint on positional parameters only, so
`is marshalled-by('Str')` picked the *Callable* candidate and mixed a `Str` into a `has &.marshaller`
slot. The named-parameter path now applies the same rule it already applied for `@` (Positional) and
`%` (Associative), and it reads the sigil off the alias for an aliased parameter (`:c(:&cb)`), whose
outer parameter is named for its external key and carries no sigil at all.

## Result

`JSON::Class` 0.0.21's `t/040-traits.t` — the one file of its six that was still red — passes, and
with it the `is marshalled-by` / `is unmarshalled-by` / `is json-skip-null` / `is json-name` traits
that anything building on `JSON::Class` (`Ddt`, `META6`, `Config::Parser::NetRC`) reaches only
through the re-export.

Pinned by `t/modules/import-export/re-exported-trait-mod.t` (with the two `t/lib` fixtures it loads)
and `t/routines/dispatch/multi-named-amp-sigil-callable.t`, both measured against raku v2026.07.
