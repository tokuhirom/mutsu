# Parser fails on `samewith(...) ; ++ $count` combined with `|c` and `$^a` inside a `.map:` block

## Repro

`Archive::SimpleZip`'s `lib/Archive/SimpleZip.rakumod` (REA `Archive::SimpleZip`
v0.8.0) has:

```raku
multi method add(Iterable:D $s, |c --> Int:D)
{
    my $count = 0;
    $s.map: { samewith($^a, |c) ; ++ $count} ;
    return $count;
}
```

Under `raku` this compiles and the dist's own suite passes (3/3 files, 38
assertions, excluding the author-only `meta.t` which needs `Test::META`).
Under mutsu, `use Archive::SimpleZip;` fails to parse:

```
Failed to parse module 'Archive::SimpleZip': Confused. parse error at line 20,
column 1: ... near: "++ $count} ;\n\n        return $count;\n    }\n\n
multi method mkdir(Str:D() $name" ...
```

The error trail (nested "expected statement" frames) points at the
`{ samewith($^a, |c) ; ++ $count}` block — a `.map:` block that uses a
`$^a` placeholder parameter, calls `samewith` (multi-dispatch redispatch) with
that placeholder plus a forwarded slurpy capture `|c`, followed by a
`;`-separated `++ $count` statement, all on one line.

## Where found

`docs/batteries/compression.md` survey (2026-08-22), measuring
`Archive::SimpleZip` (a zip-write battery candidate) — blocks `use
Archive::SimpleZip` entirely, so its whole suite is 0/N under mutsu vs 3/3
under raku.

## Not yet isolated further

A hand-written minimal repro (class with a `|c` slurpy method, a `.map:`
block using `$^a` + `samewith` + a trailing `; ++ $var`) did not reproduce the
parse failure in isolated testing — the trigger needs more of the surrounding
file (possibly interacting with an earlier construct in the same file, or a
`unit module`/dynamic-`EXPORT`-imported symbol also being referenced nearby,
similar in flavor to
[compress-bzip2-ternary-parse-after-dynamic-export.md](compress-bzip2-ternary-parse-after-dynamic-export.md)).
Whoever picks this up should start from the real file
(`Archive::SimpleZip.rakumod`, fetchable from
`https://raw.githubusercontent.com/raku/REA/main/archive/A/Archive%3A%3ASimpleZip/`)
and bisect it (e.g. binary-search which earlier declarations are load-bearing
for the failure to reproduce) rather than re-guessing a repro from scratch.

## Affected files

`src/parser/` — likely the block-signature / placeholder-parameter or
`samewith`+slurpy-capture parsing path.
