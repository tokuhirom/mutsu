# `cglobal` accepts an undefined library name, and NativeCall accepts a `(name, version)` library

Found during the XML battery survey (`docs/batteries/xml.md`) while running
`LibXML`'s own `t/000sanity.t`. `LibXML::Raw::Defs` declares

```raku
our $CLIB is export(:CLIB) = Rakudo::Internals.IS-WIN ?? 'msvcrt' !! Str;
```

and then probes `ok(try {$CLIB.&cglobal($_, Pointer)}, "clib $_ binding")` for
`<malloc memcpy free>`. On Linux `$CLIB` is deliberately the **undefined `Str`
type object** — the documented way to say "resolve this symbol against the
process-global namespace", written by every binding that either dlopens its
library itself first or binds already-linked libc symbols. mutsu answered:

```
Cannot locate native library 'lib(Str).so': lib(Str).so.2: dlopen failed
```

## Root cause

`is native(<undefined>)` already had this right — `vm_register_sub_ops.rs` mapped
an undefined trait argument to "no library", i.e. `dlopen(NULL)`. `cglobal` had
its own, simpler resolution in `nativecall_global.rs` that just called
`to_string_value()` on whatever it was given, so the `Str` type object
stringified to `(Str)` and went into the `lib<NAME>.so` filename template.

Rather than duplicate the undefined check, both call sites now share one
resolver, `nativecall::library_name_from_value`. Giving it a name forced the
second half of the documented contract to be handled too.

## The `(name, version)` form came with it

`Language/nativecall.rakudoc` documents two more spellings that mutsu was
silently mishandling, and the same resolver now covers them:

```raku
sub foo1 is native('foo', v1) { * }        # libfoo.so.1
my List $lib = ('foo', 'v1');
sub foo3 is native($lib) { * }
```

`Archive::Libarchive::Raw` uses the List form — `constant LIB = ('archive',
v13)` — and mutsu stringified the whole List, producing the file name
`libarchive 13.so`. That alone made every one of its native subs unusable. A
`(name, version)` List is now decorated into the one versioned file name it
denotes (`libarchive.so.13` on Linux, `libarchive.13.dylib` on macOS), which is
what Rakudo does; the version *replaces* the candidate list rather than extending
it, because the whole point of writing an ABI version is that the undecorated
`libfoo.so` is often absent on a runtime-only system.

The two-argument trait spelling needed a parser fix as well: `parse_sub_traits`
parsed only the first expression inside a trait's parentheses and discarded the
rest, so the `v1` in `is native('foo', v1)` never reached the runtime. It now
re-parses the argument list as a parenthesised list when a comma remains —
scoped to `native`, the one trait here that documents a second argument.

While the library-name path was open, `resolve_library_candidates` also became
platform-aware: `libSystem.dylib` rather than `libc.so.6` for the C runtime on
macOS, and `libfoo.dylib` / `libfoo.N.dylib` candidates instead of `.so` ones.

## Pins

`t/nativecall-pointer-and-cglobal.t` covers `cglobal` with an undefined `Str`
library (both the `.&cglobal` and direct call forms), a missing symbol still
failing, `is native(<undefined>)`, and both versioned spellings reaching the
loader with their version in the file name. `library_name_tests` in
`src/runtime/nativecall.rs` pins the per-platform decoration.
