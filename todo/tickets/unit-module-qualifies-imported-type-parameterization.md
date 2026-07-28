# A `unit module` qualifies an *imported* type, breaking its parameterization

Inside a `unit module`, a type that came from `use` is looked up under the
module's own package before the import, so parameterizing it fails:

```raku
# tmp/qlib/QMod.rakumod
unit module QMod;
use NativeCall;
sub make-ptr() is export {
    my \t = uint8;
    Pointer[t];
}
```

```
$ mutsu -I tmp/qlib -e 'use QMod; say make-ptr().^name;'
QMod::Pointer cannot be parameterized
  in sub make-ptr at tmp/qlib/QMod.rakumod line 1

$ raku -I tmp/qlib -e 'use QMod; say make-ptr().^name;'
NativeCall::Types::Pointer[uint8]
```

The bare (unparameterized) `Pointer` resolves fine — only the `[...]`
parameterization takes the qualified path. The **block** form of the same
module (`module QMod { ... }`) is unaffected, so this is specific to how
`unit module` sets the current package for the statements that follow it.

Reversing the order (`use NativeCall;` before `unit module QMod;`) is not a
workaround — `unit module` must be the first statement.

## Why it matters

It is what now stops `NativeHelpers::Blob`'s `pointer-to` / `BPointer`, and with
it `DBIish`'s mysql driver — the ⑨ row of
[`dbiish-blockers.md`](dbiish-blockers.md). Both `NativeHelpers::Blob` and
`MoarVM::Guts::REPRs` are `unit module`s that `use NativeCall` and write
`Pointer[t]`:

```
$ mutsu -I <NativeHelpers-Blob>/lib -e 'use NativeHelpers::Blob; BPointer(Buf.new(1,2,3))'
NativeHelpers::Blob::Pointer cannot be parameterized
  in sub BODY_OF ...
  in sub pointer-to ...
  in sub BPointer ...
```

This is *progress*, not a regression: the ledger's recorded symptom for ⑨ was a
parse failure in `DBDish::mysql::StatementHandle` caused by an undeclared
`BPointer`. With [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
P0/P1 landed, `BPointer` now resolves and runs all the way into `BODY_OF`; this
name-qualification bug is what it hits there.

## Where to look

`SetCurrentPackage` and the unit-module package scoping added in #5369/#5370/
#5373 — the type-parameterization path presumably resolves the base name
through the current package without first consulting the lexical import, while
the plain type-object lookup consults the import. Making the two agree (import
first, then package) is the likely fix; check it against
`t/lib-path-precedence.t` and the `unit module` pins from those PRs.
