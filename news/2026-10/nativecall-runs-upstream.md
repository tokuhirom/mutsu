# `use NativeCall` loads the vendored upstream module; the native provider is gone

`use NativeCall` and `use NativeCall::Types` used to be intercepted by name and served by a
native provider (a Rust-side `Pointer`/`CArray`/`void`/`OpaquePointer` prelude, six helper
subs, and a by-name path that consumed every `is native` routine without running any Raku).
Both names now load the vendored Rakudo sources in `modules/Rakudo-Core/lib/`, verbatim
(ADR-11203, #11203): `META6.json` provides them, and `is native` applies upstream's own
`Native` role, whose call is `nqp::nativecall` over the routine's `is box_target` callsite.

The REPR and FFI work behind that landed in earlier slices (#11204-#11211, #11753, #12029,
#12052, #12113, #12124); this change flips the switch and fixes what the real names exposed:

- ADR-0056's bare-key display qualification is removed. It rewrote
  `NativeCall::Types::size_t` to the core builtin `size_t`, so the type lost its `native`
  declaration (`.REPR` answered `P6opaque`, `.^nativesize` 64 instead of -6) and upstream's
  `check_routine_sanity` rejected `size_t`/`long` parameters. The `CArray` row of the core type
  catalog went with it: `CArray` is declared by `NativeCall::Types`, not by the core.
- A constant that aliases a type takes a smiley in an expression (`CArray:D`, `Pointer:U`), and
  a signature resolves a *parametric* alias in the scope that declares the routine
  (`CArray[int32]`, `Pointer[void]`), as Rakudo stores the type object there.
- A hoisted routine that re-arrives with the written return spelling of an alias (`--> Pointer`)
  is the declaration the in-sequence pass installed, so an `is native` sub inside a parametric
  role body (`NativeHelpers::CStruct`'s `LinearArray[T]`, which DBIish's mysql driver loads)
  no longer dies with "Redeclaration of routine 'calloc'" the second time the role is
  instantiated.
- A `Pointer` field declared through the alias reads back as upstream's type, not as a bare
  handle (`MoarVM::Guts::REPRs`' `MVMArrayB.any`, which `NativeHelpers::Blob.pointer-to` reads).
- A call through an imported multi dispatcher (`&trait_mod:<is>` exported by NativeCall's
  `sub EXPORT`) whose family has no candidate for the call retried through itself forever and
  overflowed the stack: `sub b is noted { }` with NativeCall loaded and no `noted` trait aborted
  the process. It now ends in the usual "Can't use unknown trait" error, and a captured multi
  none of whose candidates applies raises `X::Multi::NoMatch` instead of the first candidate's
  own binding error.

What the bundled-library suites (DBIish, NativeHelpers::Blob/CStruct, OpenSSL, NativeLibs,
IO::Socket::SSL) then found under the real names, all fixed as general behaviour:

- A constant naming a parameterization (`constant OidArray = CArray[Oid]`) constrains
  parameters and variables as the type it names; before, `sub f(OidArray $a)` accepted any
  `CArray` and `my OidArray $x` was "Type 'OidArray' is not declared".
- The element of a native `CArray` answers an `IntPosRef`/`NumPosRef` container, as upstream's
  `AT-POS ... is raw` does. A plain read (`$c[0]` in an expression, `^$c[0]`, `1..$c[0]`, a
  built-in method argument such as `$blob.subbuf(0, $c[0])`) reads the number in it; a bind,
  an `is rw` argument and `.VAR` keep the container.
- A role mixin over an object keeps the class's own `Numeric`, `succ` and `pred`, so
  `NativeHelpers::Pointer`'s `$p - $q` and `$p++` work on a typed `Pointer[T]` mixin.
- `buf8`, `blob16`, ... report `Uninstantiable` as their `.REPR` and `utf8`/`utf16` report
  `VMArray`, which `NativeCall::validnctype` accepts.
- Resolving a lexical type by its source name takes its declaration, never one of its curried
  specializations: once `TypedCArray[Str]` had been registered, a bare `TypedCArray` reached from
  a module loaded by a `require` inside a sub named that specialization and `CArray[Str]` died
  in `^parameterize`.
- A parametric role's body ran its `sub` declarations in the package of the *first* method of a
  `HashMap`, so on some runs (a synthesized `handles` delegate is declared in `GLOBAL`) an
  `is native` routine in the body was declared where the role's own trait candidates are not in
  scope and its second construction called the declared `{ * }` body. The package is now chosen
  by method name, preferring one a method was actually declared in. This was a flake of ~25%
  on `t/nativecall/require-in-method-cstruct-role-statics.t`.
- A call through an imported multi dispatcher whose family has no matching candidate no longer
  re-enters the dispatcher while a call through it is already running (an unknown trait with a
  user `trait_mod:<is>` multi beside NativeCall's overflowed the stack), and `.candidates` of a
  dispatcher counts its captured candidates once per declaration.
- A definiteness smiley on a lexical class reached through a constant alias is kept in the name
  (`(Bar:D).^name` is `Foo:D`).

Known gap, filed as [#12161](https://github.com/tokuhirom/mutsu/issues/12161): a nested module's
exported `proto`/`multi` (and so NativeCall's `our sub` helpers) stay visible to the using
scope through a symbolic `::('&name')` lookup, where Rakudo hides them. The native provider
used to hide NativeCall's five helpers; `t/modules/block-use-keeps-nested-module-imports.t`
lost its last assertion for that.

Deleted: the provider prelude (`NATIVECALL_POINTER_PRELUDE`, `NATIVECALL_SUB_PRELUDES`,
`TRAIT_MOD_IS_NATIVECALL_PRELUDE` and their injectors), `register_nativecall_exports`, the
`__mutsu_nativecast`/`__mutsu_nativesizeof`/`__mutsu_cglobal_fetch`/`__mutsu_explicitly_manage`
by-name helpers, `register_native_call_sub`/`register_native_call_method`, `native_call_specs`
and every call opcode, method dispatch and code-object path that consulted it, the
function-pointer module, `explicitly_managed_address`, and `scripts/nativecall-upstream-trial.sh`.

Tests that pinned the provider's surface now state Rakudo's behaviour (each verified with
`raku`): `nativecall-explicitly-manage.t` (the managed buffer is a `CStr`-REPR object reached
through `.cstr` where there is one, and `nqp::unbox_s` reads it), `nativecall-types-module.t`
(`use NativeCall::Types` alone exposes only the qualified names), `carray-native-storage.t`
(`CArray[Str].REPR` is `CArray`) and `nativecall-pointer-param-and-carray-allocate.t`
(`CArray[Str].allocate` dies in Rakudo 2026.09, so the reference-element case uses `Pointer`).

Still open: [#12144](https://github.com/tokuhirom/mutsu/issues/12144), a pre-existing flake in
`t/nativecall/nativecall-mvp.t` (an `is native('m', v6)` trait argument occasionally evaluates to
the previous routine's `('c', v6)`; the pre-switch binary reproduces it), and the
marshaller's provider-era fallbacks (`make_native_handle`, the name-keyed `CArray` branches),
which upstream's types never reach.
