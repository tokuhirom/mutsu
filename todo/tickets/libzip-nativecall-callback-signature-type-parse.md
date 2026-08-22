# Parser fails on a native-callback `&(...)` signature type used as a NativeCall parameter type

## Repro

`LibZip`'s `lib/LibZip/NativeCall.pm6` (REA `LibZip` dist) declares a native
sub that takes a C function pointer (callback) as a parameter, typed with an
anonymous `Callable` signature and a trailing comment on the next line:

```raku
sub zip_source_function(zip                   # zip*
                       ,& (Pointer, Pointer, int64, int32 --> int64) # Typedef<zip_source_callback>->...
                       ,Pointer                        # void*
                       ...
                       ) is native(LIB) is export { * }
```

Under `raku`, `use LibZip;` compiles cleanly. Under mutsu:

```
$ mutsu -I lib -e 'use LibZip; say "ok"'
===SORRY!=== Error while compiling -e
expected statement: expected ')'
at -e:498
```

(line 498 is inside `LibZip::NativeCall`, reached transitively through `use
LibZip`; the reported line number belongs to the imported module, not the
`-e` snippet — worth checking mutsu's error-location reporting separately if
it turns out to consistently misattribute lines across `use` boundaries).

## Where found

`docs/batteries/compression.md` survey (2026-08-22), measuring `LibZip` (a
zip-file NativeCall binding candidate). Blocks `use LibZip` entirely — the
dist's own (very thin, 2-assertion) test suite is 1/1 under raku and 0/1
under mutsu (fails to parse before running).

## Affected files

Parser handling of NativeCall parameter-type syntax — search
`src/parser/` for how `&(...)` / anonymous-Callable-signature parameter types
are parsed (this is distinct from a `Callable` *variable* type; here it's
used inline as a parameter type in a native sub signature).

## Priority note

`LibZip` has 0 known dependents and the thinnest test suite (2 trivial
assertions) of any candidate in the compression/archive survey, so this is
lower priority than the `Archive::Libarchive::Raw` CPointer-return-type bug
or the `Compress::Zlib` shadowing bug — recorded for completeness, not as a
next pick.
