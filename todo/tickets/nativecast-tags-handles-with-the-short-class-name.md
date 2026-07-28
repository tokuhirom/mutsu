# `nativecast` tags a handle with the class's short base name

```raku
# M4.rakumod
unit module M4;
use NativeCall;
my class BB is repr('CStruct') {
    has uint64 $.elems;
    method realstart(::?CLASS:D:) { "RS" }
}
our sub mkBB($p) is export { nativecast(BB, $p) }
```
```raku
use M4; use NativeCall;
say BB.^name;                    # M4::BB   (both)   -- the class itself is fine
my $h = mkBB($p);
say $h.^name;                    # raku: M4::BB      mutsu: BB
say (try $h.realstart).raku;     # raku: "RS"        mutsu: Nil (No such method)
```

`Interpreter::try_nativecast` (`src/runtime/cstruct_layout.rs:566`) shortens the
target type with `short_base_name` and hands that to `make_native_handle`, so a
CStruct/CPointer/CUnion class declared inside a module is tagged with a name
that does not match its own registration. Every explicitly declared method on it
then fails to resolve.

Generated *accessors* already work: they are resolved through
`cstruct_class_name`, which falls back from the short name to the registered one
(`t/nativecall-cstruct-fields.t` pins this, including the module case). It is the
hand-written methods that have no such fallback — and `.^name`, which simply
answers the wrong thing.

## Why it matters

It is what stops `DBIish`'s mysql driver one step short of running a query.
`MoarVM::Guts::REPRs` declares

```raku
my class MVMArrayB is repr('CStruct') {
    has uint64 $.elems; has uint64 $.start; has uint64 $.ssize; has Pointer $.any;
    method realstart(::?CLASS:D:) { +$!start ?? Pointer.new(…) !! $!any }
}
```

and `NativeHelpers::Blob`'s `pointer-to` is `BODY_OF(blob).realstart`. With the
connection now working
([news](../../news/2026-07/nativecall-cglobal-and-native-methods.md)),
`$dbh.prepare` dies here.

## The fix, and its risk

Tag the handle with the **registered** class name instead of the short one, so
`.^name` matches raku and ordinary method resolution finds the class. The
resolved name is already in hand: `try_nativecast` reads it off the target
`Package`/`Instance` before shortening it.

`short_base_name` still has a job — it exists so `Pointer[MoarVM::Guts::REPRs::CStructB]`
shortens to `CStructB]`-free `Pointer[…]` handling and so `CArray[X::Y]` keeps
its parameter — so the change is "use the qualified name for the *class tag*",
not "delete the shortening".

Blast radius is the reason this is a ticket and not a one-liner: OpenSSL,
IO::Socket::SSL and the TLS battery all pass CStruct handles around, and several
places compare `.^name` or look classes up by short name. Land it with the
NativeCall `t/` files and the OpenSSL battery gate as the check.

## Its sibling: `$!attr` in a CStruct class's own method

```raku
my class B is repr('CStruct') { has uint64 $.a; method rs(::?CLASS:D:) { $!a } }
say nativecast(B, calloc(1,16)).rs;
# raku:  0            (reads the C struct)
# mutsu: Nil          ("Use of Nil in string context")
```

Only the generated accessor path goes through `cstruct_field_value`; a direct
`$!a` inside the class's own method reads the (absent) attribute storage. Both
halves are needed for `MVMArrayB.realstart`, whose body is exactly
`+$!start ?? … !! $!any`, so fix them together.
