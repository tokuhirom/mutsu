# Parser misparses a `?? BAREWORD !! BAREWORD` ternary as a call to `use`d dynamic-EXPORT constants

## Repro

`Compress::Bzip2.pm6` (REA `Compress::Bzip2` v0.4.1) does `use
Compress::Bzip2::Raw;`, and `Compress::Bzip2::Raw.pm6` re-exports its
constants (`BZ_RUN`, `BZ_FLUSH`, ...) via a **dynamic `sub EXPORT`** that
introspects its own package with `MY::`:

```raku
# in Compress::Bzip2::Raw.pm6
my constant BZ_RUN = 0;
my constant BZ_FLUSH = 1;
...
my %all-symbols = MY::.grep({ .key ~~ /:i 'bz'|'name'/ || .key eq '&fopen'|'&fclose' });
sub EXPORT { ... }   # builds the export map from %all-symbols dynamically
```

`Compress::Bzip2.pm6` then uses one of those constants in a ternary passed as
a NativeCall argument:

```raku
$!bzret = BZ2_bzCompress($!stream, ($!stream.avail-in) ?? BZ_RUN !! BZ_FLUSH);
```

Under `raku`, `use Compress::Bzip2;` compiles fine (and the dist's own suite
is 10/10 under raku). Under mutsu:

```
$ mutsu -I lib -e 'use Compress::Bzip2; say "ok"'
===SORRY!=== Error while compiling -e
Failed to parse module 'Compress::Bzip2': Your !! was gobbled by the
expression in the middle; please parenthesize
at -e:168
```

(line 168 is the `?? BZ_RUN !! BZ_FLUSH` line quoted above, reached through
the transitive `use`, not a literal line 168 of the `-e` string).

## Working hypothesis

The `Your !! was gobbled` message is the standard Raku parser error for "an
unrecognized bareword before `!!` was greedily parsed as a listop/function
call, consuming the `!!` and everything after it as arguments" — i.e. it
looks exactly like what happens when `BZ_RUN` is **not yet known to the
parser as a declared constant** at that point, so the parser falls back to
treating it as a call. That would point at the same underlying gap as
[nativecall-sizeof-cstruct-repr-unsupported.md](nativecall-sizeof-cstruct-repr-unsupported.md)'s
sibling area — dynamic `sub EXPORT` built from `MY::` package-symbol-table
introspection (rather than a static list of `is export`-tagged declarations)
not registering its exported names as parse-time terms in the importing
file.

This hypothesis is **not confirmed**: a standalone check that
`use Compress::Bzip2::Raw; say BZ_RUN;` works fine under mutsu (prints `0`),
so the dynamic `EXPORT` mechanism does succeed in a simple case. The failure
only reproduces inside the full `Compress::Bzip2.pm6` file; several
hand-reduced repros (ternary in a NativeCall arg, inside a `class` method,
inside a `repeat {...} while (...)` loop, with a typed `int32` attribute) did
**not** reproduce it. Whoever picks this up should bisect the real file
(fetchable via
`https://raw.githubusercontent.com/raku/REA/main/archive/C/Compress%3A%3ABzip2/`)
rather than re-guessing — something earlier in the file (the `X::Bzip2 is
Exception` class with a `given/when` over the same dynamically-imported
constants, lines ~7-30) is a candidate for what makes the difference.

## Where found

`docs/batteries/compression.md` survey (2026-08-22), measuring
`Compress::Bzip2` (bzip2 compression battery candidate) — blocks `use
Compress::Bzip2` entirely. raku: 1/1 file (10 assertions). mutsu: 0/1 (fails
to parse).

## Affected files

`src/parser/` (ternary / bareword-before-`!!` disambiguation) and possibly
the same dynamic-`EXPORT`-via-`MY::` machinery as the sizeof-CStruct ticket.
