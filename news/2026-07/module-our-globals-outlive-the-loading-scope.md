# A module's `our` globals outlive the scope that loaded it

Three fixes from the `DBIish` ledger, each a general-purpose gap. Together they
take `DBIish`'s `t/01-basic.rakutest` from 18 of 35 subtests to 30, and make
`NativeHelpers::CStruct` loadable.

## `our` package variables are not lexical

`DBIish.install-driver('Pg')` followed by `install-driver('SQLite')` died with
`Could not find symbol '&is-win' in 'NativeLibs'`, raised from inside
`NativeLibs`' own `CHECK`. Installing either driver on its own worked; only the
pair failed, and a plain top-level `require ::('DBDish::Pg')` before the second
one did not reproduce it.

Reduced, it is three lines and does not need a database:

```raku
sub f() { my \M = (require ::('Base')); }   # Base has `our constant flag = 7`
f();
use Base;
say Base::flag;          # raku: 7    mutsu: could not find symbol
```

`our` declarations compile to a `SetGlobal` on the qualified name, so they live
in `env` — and a sub call restores `env` wholesale on return. A module loaded by
a `require` inside a sub therefore lost its package variables when the sub
returned, while `loaded_modules` kept the module marked as loaded, so the later
`use` was a no-op that could not bring them back. That is the same shape as the
subtest and `EVAL` registry-rewind bugs fixed earlier this month, one carrier
down: the routine registry already had `module_registered_functions` to survive
exactly this, but `our` variables are not routines.

Each module load now records the package-qualified globals it introduced, and the
already-loaded path of `use_module_with_tags_inner` reinstates whatever has gone
missing since. Only missing keys are put back, so an assignment made after the
load is never clobbered. In `DBIish` this is what lets the second driver's
`use NativeLibs` see `NativeLibs::is-win` again.

## The C-width native integer aliases are declarable

`DBDish::mysql::Native` writes `has ulong $.length` and
`our ulong constant ulong_zero = 0`, and mutsu answered `Type 'ulong' is not
declared`. The marshalling layer in `runtime/nativecall.rs` already mapped
`long` / `ulong` / `longlong` / `ulonglong` / `size_t` to `CType::I64` /
`CType::U64` — only the declaration side was missing, so nothing could *name*
one. They are in `NATIVE_INT_TYPES` now, with 64-bit bounds, wrapping and
signedness, and they name a type object as a term so `nativesizeof(ulong)`
reports 8, matching MoarVM on the platforms mutsu targets.

## A role type parameter can carry a definiteness smiley

`role LinearArray[::T] { … multi method Pointer(::?CLASS:U: T:D $struct) … }` —
`NativeHelpers::CStruct`'s central role — failed to register with `Invalid
typename 'T:D' in parameter declaration.`, so the whole module was unloadable.
Role-method registration compared the *whole* constraint against the role's
type-parameter list, so a bare `T` matched but `T:D` did not; the base name is
already computed a few lines further down for exactly this kind of stripping.
`NativeHelpers::CStruct` loads now.

## What still blocks the mysql driver

`01-basic`'s remaining three failures are all the `mysql` driver, and it is
gated on the deferred
[`todo/deep/nativehelpers-blob-moarvm-guts.md`](../../todo/deep/nativehelpers-blob-moarvm-guts.md)
work rather than on anything new: `DBDish::mysql::StatementHandle` uses
`BPointer(...)`, which is `NativeHelpers::Blob`'s `pointer-to` and needs
`BODY_OF` — the address of a container's element buffer, stable across calls.
The SQLite driver does not go through it, which is why the other eight files are
unaffected.
