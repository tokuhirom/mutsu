# A CStruct handle carries its registered name, and its methods can read `$!`

Two halves of the same gap, fixed together because neither is useful alone.

## `nativecast` tagged the handle with the short base name

```raku
# CStructInModule.rakumod
unit module CStructInModule;
my class Body is repr('CStruct') {
    has uint64 $.elems; has uint64 $.start;
    method describe(::?CLASS:D:) { "elems=$!elems start=$!start" }
}
our sub make-body($p) is export { nativecast(Body, $p) }
```
```raku
say Body.^name;               # CStructInModule::Body   (both — the class was fine)
say make-body($p).^name;      # raku: CStructInModule::Body   mutsu was: Body
say make-body($p).describe;   # raku: works                   mutsu was: No such method
```

`try_nativecast` shortened the target type with `short_base_name` and handed
that to `make_native_handle`, so a CStruct/CPointer/CUnion declared inside a
module was tagged with a name that did not match its own registration. Every
explicitly declared method on it failed to resolve, and `.^name` simply answered
the wrong thing.

Generated *accessors* already worked — they resolve through `cstruct_class_name`,
which falls back from the short name to the registered one (that fallback landed
with the `VMArray` REPR body). It was the hand-written methods that had no such
path. The handle now carries the registered name, so ordinary method resolution
finds the class and `.^name` matches raku. `short_base_name` keeps the job it
was introduced for: shortening the *parameter* forms (`Pointer[T]`,
`CArray[X::Y]`).

## `$!field` inside the class's own method read nothing

```raku
my class B is repr('CStruct') { has uint64 $.a; method rs(::?CLASS:D:) { $!a } }
say nativecast(B, calloc(1, 16)).rs;
# raku:  0        mutsu was: Nil  ("Use of Nil in string context")
```

A CStruct handle keeps its fields in native memory, not in the attribute cell,
and only the generated `$obj.field` accessor went through `cstruct_field_value`.
A direct `$!field` read the (absent) attribute storage.

The declared fields are now materialised into the cell at method entry —
**re-read on every entry**, not cached once, because the authoritative copy is
the C struct and a callee may have changed it in between. `t/nativecall-cstruct-in-module.t`
pins that: it writes a field through a native `memcpy` and both the accessor and
the `$!`-reading method see the new value.

## Why both, and why now

`MoarVM::Guts::REPRs` needs exactly this pair. It declares
`my class MVMArrayB is repr('CStruct')` **inside a module** and reads it with

```raku
method realstart(::?CLASS:D:) { +$!start ?? Pointer.new(+$!any + …) !! $!any }
```

so a short-named handle loses the method, and a method that cannot read
`$!start` is useless even once found. `NativeHelpers::Blob`'s `pointer-to` is
`BODY_OF(blob).realstart`, which is on `DBIish`'s mysql `prepare` path — the
next thing after the [connection](nativecall-cglobal-and-native-methods.md)
itself.

Pinned by `t/nativecall-cstruct-in-module.t` (7 tests), which passes identically
under raku.
