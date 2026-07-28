# A `unit module` no longer captures the builtin preludes

Three builtin type declarations are not written in Rust but in Raku, and are
spliced into whichever compilation unit references them: NativeCall's `Pointer`
(and its companion `void`), the parametric `Rational` role, and the `IO::Socket`
role. `inject_nativecall_prelude` and friends prepend the parsed prelude to the
file's statement list, and until now they were written with bare names —
`class Pointer { … }`.

That was fine for a mainline program and wrong for a module. For a
`unit module M` file the compiler emits `SetCurrentPackage M` at the *top* of the
compilation unit — ahead of the sub-hoist pass, deliberately, so the module's own
routines register as `M::name` rather than leaking into `GLOBAL::` (PLAN 8.22).
The spliced prelude sits inside that unit, so its `class Pointer` registered as
`M::Pointer`: a *different* type from the builtin one, with none of its
behaviour attached. Parameterizing it failed outright —

```
$ mutsu -I lib -e 'use NativeHelpers::Blob; BPointer(Buf.new(1,2,3))'
NativeHelpers::Blob::Pointer cannot be parameterized
```

— because `is_non_parametric_type` allow-lists the name `Pointer`, and
`NativeHelpers::Blob::Pointer` is not it. The bare (unparameterized) type object
resolved fine, which is what made the failure look so arbitrary: only the `[T]`
form took the package-qualified path.

The fix is to say what was meant. Class and role registration already strip a
`GLOBAL::` prefix (`class GLOBAL::Foo` declares `Foo` in the global namespace,
whatever package encloses it), so the three preludes now declare
`GLOBAL::Pointer`, `GLOBAL::void`, `GLOBAL::Rational` and `GLOBAL::IO::Socket`.
They land globally regardless of what the host file declares, which is the only
thing a *builtin* type can sensibly mean.

Making the preludes use that escape hatch exposed a gap in it. The parameter-type
pre-pass (`eval_check::collect_declared_type_names_with`) harvests declared type
names straight from the AST, and recorded only the name as written — so after the
change a `sub f(Pointer $p)` in the very same file was rejected with "Invalid
typename 'Pointer' in parameter declaration", since the unit declared
`GLOBAL::Pointer`. Declared names are now recorded under both spellings, which is
independently correct for user code: `class GLOBAL::Foo { }; sub f(Foo $x) { }`
is legal Raku and was rejected before.

Pinned by `t/unit-module-prelude-type-global.t`, which passes identically under
`raku` — with one deliberate exception. mutsu names the prelude class plain
`Pointer` where raku says `NativeCall::Types::Pointer`, at global scope as well as
inside a module, so the test asserts the base name rather than the whole of it.
That naming gap is tracked separately in
`todo/tickets/nativecall-pointer-short-name.md`.

This is the ⑨ blocker of the `DBIish` battery ledger: `BPointer` now runs past
type parameterization and into `BODY_OF` itself, where what stops it is
ADR-0015 P2 — `Buf`/`Blob` still answer `P6opaque` to `.REPR`, so
`MoarVM::Guts::REPRs` finds no body type for them.
