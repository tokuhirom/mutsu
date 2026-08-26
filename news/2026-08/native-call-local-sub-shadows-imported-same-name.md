# A locally-declared sub now shadows a same-named imported NativeCall sub

`Compress::Zlib.pm6` (from the `Compress::Zlib` REA dist) does `need
Compress::Zlib::Raw;` and then declares its own `our sub compress(Blob $data,
Int $level = 6 --> Buf) is export { ... }`, a 1-2-arg high-level wrapper
around `Compress::Zlib::Raw`'s 4-arg native `sub compress(...)`. Under
`raku`, calling `compress($data)` from inside `Compress::Zlib.pm6` resolves to
the local wrapper, because a same-scope declaration shadows a same-named
imported/needed symbol. Under mutsu it resolved to the imported 4-arg native
`compress` instead, dying with `NativeCall: 'compress' expects 4 argument(s),
got 1`.

## Root cause

This was a genuine dispatch-resolution bug, not a NativeCall-specific one —
NativeCall only made it *detectable*, because the wrong candidate has an
obviously mismatched arity.

`native_call_specs` (the interpreter's C-FFI descriptor table for `is
native(...)` subs) was a single flat, **unscoped** `HashMap<String,
NativeCallSpec>` keyed only by a sub's bare (short) name and, when known at
registration time, its `pkg::name` qualified form. Every ordinary function
call site (`exec_call_func_op`, `exec_exec_call_op`,
`try_dispatch_native_by_name`) consulted this table by bare name **before**
any of the normal lexical/package resolution ran. Because the table carries
no notion of lexical scope, whichever native descriptor last claimed a given
bare name won for every subsequent bare call to that name anywhere in the
program — including from a file that had declared its own, unrelated, later
(or even textually earlier, since Raku hoists sub declarations) plain sub of
the same name.

Attempting to fix this at *registration* time (removing the bare
`native_call_specs` entry whenever a plain sub of the same name is installed)
turned out to be order-dependent and unreliable: Raku's own hoisting means a
module's local subs are pre-registered, stripped of most traits, before any
of its top-level statements (including the `need`/`use` that pulls in the
native descriptor) run; the *real*, trait-carrying registration of that same
local sub later in sequence is then an idempotent no-op that never re-runs
the cleanup.

## Fix

The fix instead resolves natives **at call time**, using the same
scope-walking order ordinary bare-name routine resolution already uses
(`bare_name_packages()`, innermost package first, ending at `GLOBAL`). The
new `Interpreter::resolve_native_call_spec(name)` walks that chain; at each
enclosing package it prefers a native descriptor registered directly under
`pkg::name` (the routine's own declaring scope). Otherwise, if a
`FunctionDef` (plain or `multi`) is registered under `pkg::name`, it traces
that `FunctionDef` to its *true* declaring package via its own `package`
field, and checks whether *that* package has a native descriptor for the
name.

That tracing step was necessary because a first cut — "a `FunctionDef` at
`pkg::name` always means a local shadow, so stop and defer to ordinary
dispatch" — broke a different, more common shape: `use`/`import`
re-exporting an already-registered routine into a new package aliases the
exact same `Arc<FunctionDef>` under the new qualified key
(`import_module`) *without rewriting its `package` field*. So a script that
does `use NCTypeAliasMod; alloc(...)` gets a `GLOBAL::alloc` `FunctionDef`
whose `package` is still `NCTypeAliasMod` — an imported *copy* of the exact
native routine, not a different local declaration — and the first cut
mistook that copy for a shadow, sending `alloc` through the (wrong) plain
stub path and regressing `t/nativecall-constant-type-alias.t` and
`t/nativecall-cpointer-class-in-module.t`. Tracing to the true owner tells
the two cases apart: `GLOBAL::alloc`'s owner (`NCTypeAliasMod`) *does* have a
native `alloc`, so it is the same routine (dispatch natively); `GLOBAL::compress`'s
owner (`Compress::Zlib`, after `use Compress::Zlib;`) has *no* native
`compress` of its own, so it is a genuinely different routine (defer to
ordinary dispatch, which then correctly finds and calls that exact
`FunctionDef` — the local wrapper). Finding neither a native nor a
`FunctionDef` at a given package falls back to the historic flat bare-name
entry, so existing NativeCall usage that never triggers the walk is
unaffected. `register_native_call_routine` now always records the
`pkg::name` key (previously skipped for `GLOBAL`), so the walk can find a
`GLOBAL`-scoped native declaration too.

All four call sites that previously did a raw `native_call_specs.get(name)`
lookup (`exec_call_func_op`, `exec_exec_call_op`, and
`try_dispatch_native_by_name`, which backs both `call_sub_value` and
`vm_call_on_value` for calls through an already-resolved code object) now go
through the shared resolver.

## Shadowing rules verified against `raku`

Measured directly against `raku` (not assumed) across the axes that matter
for this shape:

- `our sub`, `my sub`, and `multi sub` local declarations all shadow a
  same-named `need`-loaded native sub, exactly like a same-named plain
  sub does.
- Declaration order relative to the `need`/`use` does not matter — a local
  sub declared *before* the `need` statement still shadows it, because Raku
  hoists every top-level sub declaration to the start of its compunit.
  A call from *within* the declaring module, and a call from the importing
  script (after `use Module;`), both resolve to the local declaration.
- A local sub does **not** need `is export` to shadow — an unexported local
  `sub foo(...)` still wins over the needed native `foo`.
- This shadowing only applies to `need` (load without importing). A plain
  `use Module;` that imports a same-named symbol, followed by a same-scope
  local declaration of that name, is a compile-time `X::Redeclaration`
  ("Redeclaration of routine ...") under `raku` regardless of whether the
  imported symbol is native or plain — mutsu does not yet implement that
  compile-time redeclaration diagnostic for import-vs-local collisions in
  general (a separate, pre-existing gap, not specific to NativeCall).
- An explicit package-qualified call (`Compress::Zlib::Raw::compress(...)`)
  is unaffected by any of this and still reaches the native routine.

## Verification

A regression test, `t/native-call-local-sub-shadows-import.t`, with fixtures
`t/lib/NativeShadowInner.rakumod` (five arity-mismatched native subs) and
`t/lib/NativeShadowOuter.rakumod` (`need`s the inner module, then declares
local wrappers of each of the five shapes above), passes identically under
`raku` and `mutsu`.

The real `Compress::Zlib` dist's `t/01-basic.t` (fetched from
`retupmoca/P6-Compress-Zlib` and `retupmoca/P6-Compress-Zlib-Raw` for
verification) now passes fully under mutsu (5/5), up from a hard failure at
test 2. `t/02-stream.t` and `t/03-wrap.t` still fail, but for unrelated
reasons (native `z_stream` CStruct/out-parameter handling) — see
`docs/batteries/compression.md` for the compression-battery survey this was
found from.
