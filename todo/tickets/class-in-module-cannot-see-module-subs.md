# A class inside a module cannot see the module's subs

A method of a class declared inside a `module` (either `unit module` or a
`module { ... }` block) cannot call a sub declared at that module's scope. The
call dies with `Unknown function: <name>`. raku resolves it.

This is the blocker behind five of the nine `DBIish` test files — see
[`dbiish-blockers.md`](dbiish-blockers.md) ② — but it has nothing to do with
`DBIish`, `NativeLibs`, `proto`/`multi` or NativeCall. It is a plain
lexical-scope bug.

## Repro

```raku
# lib/NL.rakumod
unit module NL;
our sub cannon-name($libname) { "cn:$libname" }
class Searcher {
    method try-versions($libname) { cannon-name($libname) }
}
```

```
$ raku  -I lib -e 'use NL; say NL::Searcher.try-versions("sqlite3")'
cn:sqlite3
$ mutsu -I lib -e 'use NL; say NL::Searcher.try-versions("sqlite3")'
Unknown function: cannon-name
```

## What the boundary is

Measured 2026-07-26 against `main`, one variant per line. raku resolves every
one of them.

| Variant | mutsu |
| --- | --- |
| `class` at **file scope**, sub at file scope | works |
| `class` inside `unit module`, `our sub` at module scope | **Unknown function** |
| `class` inside `unit module`, plain (lexical) `sub` at module scope | **Unknown function** |
| `class` inside `unit module`, `our proto` + `multi` at module scope | **Unknown function** |
| `class` inside a `module { ... }` **block** | **Unknown function** |
| Same call from a module-scope **sub** instead of a method | works |
| Module-scope **`my` variable** read from the method | works |
| The same call written **fully qualified** (`NL::cannon-name(...)`) | works |

Two of those rows are the whole diagnosis:

- The module-scope **variable** is visible, so the method does run with the
  enclosing lexical environment available. What fails is specifically **bare-name
  function lookup**.
- The **fully qualified** call works, so the sub *is* registered — as
  `NL::cannon-name`. The method body compiles under package `NL::Searcher`, and
  bare-name lookup evidently tries that package and then GLOBAL, without walking
  the enclosing package chain in between.

So the fix is to make bare-name function resolution inside a class body walk
outward through the enclosing packages (`NL::Searcher` → `NL` → `GLOBAL`) rather
than jumping straight to GLOBAL. The file-scope row works only because there the
enclosing package *is* GLOBAL, which is why this never showed up in ordinary
single-file tests.

Whether the walk belongs in the compiler (resolve the name at compile time,
where the package nesting is known — cf. `SetCurrentPackage` and
`current_package()`) or in the runtime fallback needs a look; the compile-time
route is preferable and matches how the qualified name is already produced.

## Not to be confused with

The earlier reductions recorded under `dbiish-blockers.md` ② — "a plain
`our proto sub` + multis called from a sibling sub", "the same with a custom
`sub EXPORT(|)`" — all pass, and correctly so: they call from a *sub*, not from a
method of a nested class. That is the row that works.

## Aside, found in the same file

`$*VM.config<nativecall_backend>` is missing, so `NativeLibs`'
`my \dyncall = $*VM.config<nativecall_backend> eq 'dyncall'` warns
`Use of uninitialized value of type Any in string context` on every run that
loads it. mutsu's `$*VM.config` has exactly two keys (`be`, `name`); raku
answers `dyncall`. Harmless — `dyncall` ends up `False`, which is what mutsu
wants — but it is noise in every DBIish run and a one-line fix.
