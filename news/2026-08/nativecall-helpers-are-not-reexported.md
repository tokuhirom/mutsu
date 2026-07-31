# A module that uses NativeCall no longer re-exports its helpers

mutsu has no `NativeCall.rakumod` to import from: the five helper routines
(`cglobal`, `nativecast`, `nativesizeof`, `explicitly-manage`, `refresh`) are
spliced into every compunit whose source names them, as a prelude. Since
[#5609](../2026-07/nativecall-exports-are-module-routines.md) each was declared
`is export`, which was load-bearing rather than decorative: a prelude is spliced
into the *host* compunit, so inside a `unit module M` a plain `our sub` registers
as `M::nativecast` — invisible to a method body running under some other package,
which is exactly the shape `NativeHelpers::Pointer` has.

The workaround had the side effect its own ticket predicted
([`todo/deep/module-package-sub-invisible-from-method-body.md`](../../todo/deep/module-package-sub-invisible-from-method-body.md)):
a module that merely *uses* NativeCall re-exported the helper to whoever used
*it*. raku does not — `use NativeLibs; say &nativecast.defined` is an undeclared
routine there — and the difference was not cosmetic. The re-exported copy landed
in the importer's scope as `GLOBAL::nativecast`, and the next compunit that used
NativeCall then tried to declare its own spliced copy under the same key:

```
Redeclaration of routine 'nativecast'. Did you mean to declare a multi-sub?
  in block <unit> at .../DBDish/SQLCipher/Connection.rakumod line 1
```

`DBDish::SQLCipher` is exactly that shape — it `use`s `NativeLibs` (which calls
`nativecast` internally) and then loads `DBDish::SQLCipher::Connection` (which
calls `nativecast` itself). Every DBIish SQLCipher test file died on it, which is
what the bundled-library gate had been reporting since the helpers stopped being
builtins. `DBDish::SQLite` escaped only because its `Connection.rakumod` happens
not to call any NativeCall helper.

The fix separates the two things `is export` was doing. The prelude declarations
drop `is export` and are stamped instead with an internal `__mutsu_prelude`
trait, following the `__our_scoped` / `__lexical_hoist` marker convention (a
`__`-prefixed trait is already excluded from `has_user_custom_traits`, so
registration never mistakes it for a user trait). A routine carrying that marker
registers under `GLOBAL` rather than the host compunit's package — the same thing
the `GLOBAL::` prefix already does for the prelude's `Pointer` / `void` /
`NativeCall::CStr` classes — and enters no module's export map. Because every
such compunit carries an identical copy, the first registration wins and the rest
return `Unchanged` instead of colliding.

So `use NativeLibs` no longer brings `nativecast` with it, a method body that
never declared the helper still reaches it, and the five SQLCipher files pass
again. What is still not raku-exact is that a *process* which loads any compunit
using NativeCall makes the helper globally callable; that is inherent to the
prelude-splicing model and is what the deep ticket's lexical-resolution fix
removes. Pinned by `t/nativecall-helpers-are-not-reexported.t`, whose first two
assertions run unmodified under rakudo.
