# `Pointer[T].deref` tags the handle with T's registered name

`nativecast(T, $addr)` was taught to tag the handle it returns with the class's
**registered** name — a CStruct declared inside a module registers as `M::BB`,
and a handle carrying the short `BB` matches neither its own class for method
resolution nor raku's `.^name`. The sibling path, `Pointer[T].deref`, kept
shortening.

That is exactly the spelling `NativeHelpers::Blob` uses to reach MoarVM's array
body:

```raku
sub BODY_OF(Mu \any) is export {
    my \type = %known-bodies{any.REPR};
    nativecast(Pointer[type], OBJECT_BODY(any)).deref;
}
```

`%known-bodies<VMArray>` is `MoarVM::Guts::REPRs`' lexical `MVMArrayB`, whose
hand-written `realstart` was therefore unreachable:

```
No such method 'realstart' for invocant of type 'MVMArrayB'
```

while `MVMArrayB.^can('realstart')` said yes and the *generated* accessors
(`.elems`, `.start`, `.any`) all worked — they have a short-name fallback, which
is why nothing looked wrong until a hand-written method was called.

`try_pointer_method` now resolves the target through `cstruct_class_name`, the
same registry lookup `nativecast` uses, falling back to the short name only when
the class is not registered.

Found while walking `DBIish` towards a real MariaDB query: with this,
`DBDish::mysql`'s `prepare` gets past its parameter-binding setup.

Pinned by `t/nativecall-deref-registered-name.t` (libc only, so CI-safe), which
runs the same three assertions over `Pointer[T].deref` with the type taken from
a hash, `Pointer[T].deref` with a literal type, and `nativecast(T, …)` as the
control.
