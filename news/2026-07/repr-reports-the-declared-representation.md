# `.REPR` reports the declared representation, not just for live handles

`.REPR` answered the representation a class was declared with only for a *live
native handle* — an instance carrying a C address. A **type object** has no
address, so it fell through to `P6opaque`:

```raku
class B is repr('CStruct') { has uint64 $.a }
say B.REPR;   # was P6opaque, raku says CStruct
```

A NativeCall binding gates on exactly that. `NativeHelpers::CStruct`'s
`LinearArray[::T]` opens with

```raku
role LinearArray[::T] does Positional[T] is export {
    die "Need a CStruct" unless T.REPR eq 'CStruct';
```

so the guard fired for every parameterisation and silently killed the role's
parameterisation: `LinearArray[MYSQL_BIND]` came out as the bare, unparameterised
`NativeHelpers::CStruct::LinearArray`, with none of the composed
`handles <AT-POS elems shape>` delegation. That is what
`DBDish::mysql`'s `prepare` builds its parameter binds on.

`declared_class_repr` now answers from the CStruct / CUnion / CPointer class
registries for a **type object**, keeping `P6opaque` for an ordinary class and
for the built-in types. Instances are untouched: a live native handle already
answered correctly, and a *Raku-constructed* CStruct — one with no C storage yet
— must keep under-reporting `P6opaque`, or `BODY_OF` would dereference whatever
`.WHERE` returned (pinned by `t/nativecall-repr-body.t`; giving such an object
real storage is ADR-0015's P3).

The other half of that guard — a *rejecting* parameterisation should die, and in
mutsu does not — is a separate gap, recorded in
[`todo/tickets/role-body-guard-not-run-on-parameterisation.md`](../../todo/tickets/role-body-guard-not-run-on-parameterisation.md).

Pinned by `t/repr-reports-declared-representation.t`.
