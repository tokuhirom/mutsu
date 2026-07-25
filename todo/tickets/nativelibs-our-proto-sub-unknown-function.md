# `NativeLibs`: `Unknown function: cannon-name` (an `our proto sub`)

Loading anything that goes through `NativeLibs`' library-name resolution dies
with

```
Unknown function: cannon-name
```

`cannon-name` is not a typo in the caller — it is the real (mis-spelled upstream)
name of an `our proto sub` declared in `NativeLibs.rakumod`:

```raku
our proto sub cannon-name(|) {*}
multi sub cannon-name(Str:D $libname, Version $version?) { … }
multi sub cannon-name(Str $libname, Cool $ver) { … }
```

## Impact

`NativeLibs` (0.0.9, `zef:raku-community-modules`, Artistic-2.0, **96
dependents**) is a runtime dependency of *both* database candidates:

- `DB::SQLite` — this is its **first** failure; all 9 of its upstream test files
  die here (raku: 9/9 pass).
- `DBIish` — the chosen battery (`docs/batteries/database.md`) also lists
  `NativeLibs` in `depends`, so this has to be fixed for the database slot
  regardless of which candidate is bundled.

## Repro

```sh
mkdir -p tmp/dbslot && cd tmp/dbslot
curl -sSL 'https://raw.githubusercontent.com/raku/REA/main/archive/N/NativeLibs/NativeLibs%3Aver%3C0.0.9%3E%3Aauth%3Czef%3Araku-community-modules%3E.tar.gz' | tar xz
# then load DB::SQLite (or any dist whose driver calls NativeLibs::Loader)
```

## What it is NOT

A plain `our proto sub` in a module works. This parses, loads and dispatches
correctly under both implementations:

```raku
# lib/ProtoMod.rakumod
unit module ProtoMod;
our proto sub cannon-name(|) {*}
multi sub cannon-name(Str:D $n) { "one:$n" }
multi sub cannon-name(Str:D $n, Int $i) { "two:$n:$i" }
our sub use-it($n) { cannon-name($n) }
```

So the trigger is narrower than "`our proto sub` is broken". The distinguishing
feature of `NativeLibs.rakumod` is its **file shape**: it opens with a custom
`sub EXPORT(|)` that builds a `Map` by introspecting `&trait_mod:<is>.candidates`,
and only *then* declares `unit module NativeLibs`:

```raku
use NativeCall;

sub EXPORT(|) {
    my $exp = &trait_mod:<is>.candidates.first: { .signature ~~ :(Routine, :$native!) };
    Map.new('NativeCall' => NativeCall, '&trait_mod:<is>' => $exp.dispatcher);
}

unit module NativeLibs:ver<0.0.9>;

our proto sub cannon-name(|) {*}
```

Start by reducing that: a custom `sub EXPORT` **before** a `unit module`
declaration, with an `our proto sub` after it. The likely story is that the
declarations following `unit module` are not being registered into the module's
package when a custom `EXPORT` is present, so the intra-module call cannot
resolve.

## Note on the diagnostic

The first line mutsu prints for these files is
`Use of uninitialized value of type Any in string context`, which is a **warning
in both implementations** and not the failure. The real error is several lines
down. Do not root-cause from the first line.
