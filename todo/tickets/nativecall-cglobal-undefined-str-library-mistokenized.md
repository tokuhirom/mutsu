# `cglobal()`/`native()` with an undefined `Str` library name tries to load a literal file instead of the process-global namespace

Found during the XML battery survey (`docs/batteries/xml.md`) while running `LibXML`'s
(`zef:dwarring`, libxml2 NativeCall binding) own `t/000sanity.t` under mutsu — this test
does not `use LibXML;` (so it does not hit the role/parser bug in
`todo/tickets/role-meta-invocant-nested-colonpair-alias-param.md`), which let it get
further and expose this separate, more general NativeCall bug.

## Root cause

`LibXML::Raw::Defs` (`lib/LibXML/Raw/Defs.rakumod` in the `LibXML` dist) sets:

```raku
our $CLIB is export(:CLIB) = Rakudo::Internals.IS-WIN ?? 'msvcrt' !! Str;
```

On Linux, `$CLIB` is deliberately left as the **undefined `Str` type object** — this is
the standard NativeCall idiom for "resolve the symbol against the current process /
global namespace" (glibc functions like `malloc`/`memcpy`/`free` are already linked into
every process, so no specific library needs to be named). `t/000sanity.t` then does:

```raku
ok(try {$CLIB.&cglobal($_, Pointer)}, "clib $_ binding")
```

for `<malloc memcpy free>`.

On mutsu this fails with:

```
Cannot locate native library 'lib(Str).so': lib(Str).so.2: dlopen failed
```

i.e. mutsu is literally trying to `dlopen` a file named `lib(Str).so` — it appears to
have stringified the undefined `Str` type object's *type name* into the library-name
template (`lib<NAME>.so`) instead of recognizing "library name is an undefined `Str`"
as the documented "use the process-global/default namespace" case.

## Minimal repro

```raku
use NativeCall;
my $CLIB = Str;
say try { $CLIB.&cglobal("malloc", Pointer) } // "FAILED: $!";
```

- `raku`: prints a `NativeCall::Types::Pointer<0x...>` (the symbol resolves against the
  process-global namespace).
- `mutsu` (`target/debug/mutsu`): `Cannot locate native library 'lib(Str).so': lib(Str).so.2: dlopen failed`

## Why this matters beyond LibXML

This is a general NativeCall gap (in the same family as the compression survey's
NativeCall findings — `docs/batteries/compression.md`'s "What blocks mutsu today"
section, e.g. `nativecall-cpointer-repr-typed-param-returns-whatever.md`), not specific
to `LibXML`. Any NativeCall binding that binds against already-process-linked libc
symbols by passing an undefined `Str`/`Any` as the library name (a common idiom to avoid
hardcoding `libc.so.6` vs `msvcrt` vs `libSystem.dylib` per platform) will hit this. It
is a secondary, independent blocker for `LibXML` beyond the role/parser bug already filed
— even once that one is fixed, `t/000sanity.t` (and potentially other files using the
same `$CLIB` pattern for `malloc`/`memcpy`/`free`) would still fail here.

## Affected files (starting point, not exhaustive)

- Wherever mutsu resolves the library-name argument of `is native(...)` / `cglobal()` —
  likely in the NativeCall support code that builds the `dlopen` path from the trait
  argument's value. It needs a branch for "argument is an undefined `Str`/type object"
  that skips `dlopen` entirely and resolves the symbol against the current process
  (`dlopen(NULL, ...)` / `RTLD_DEFAULT` equivalent) instead of stringifying the type
  object into a filename template.

Not root-caused further within this survey's time budget.
