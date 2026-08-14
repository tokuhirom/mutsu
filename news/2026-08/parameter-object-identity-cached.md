# `Parameter` objects now have a stable identity for subs and closures

`Signature.params` used to build a fresh `Parameter` `Instance` on every
access, so a mixin applied to one materialization vanished on the next read:

```raku
role Q { }
sub f(:$mp) { }
my $p = &f.signature.params[0];
$p does Q;
say $p ~~ Q;                       # True  (does mutates the object now)
say &f.signature.params[0] ~~ Q;   # raku: True   mutsu: was False, now True
```

`$sig.params[0] === $sig.params[0]` was False for the same reason. The
*trait* case this ticket was originally filed for was already fixed (see
`news/2026-08/param-trait-mixin-persists.md`); this closes the "honest"
route (b) that ticket named but did not implement: materialize a callable's
`Signature` (params included) once, and hand out the same cached `Value` on
every later `.signature` read.

## Root cause

`Interpreter::sub_signature_value` (`src/runtime/methods_signature_candidates.rs`)
rebuilt a brand new `Signature` Instance — and therefore a brand new
`Parameter` array — from the `SubData` on every single call. Root cause one
level deeper: even a *repeated bareword lookup of the same declared sub*
(`&f` evaluated twice) constructs a brand new `SubData` each time (a fresh
`id`, a fresh address — verified with `.WHERE`), so there was no
already-existing object to key a cache on by identity alone.

## Fix

Added a process-global cache (`SUB_SIGNATURE_CACHE` in
`src/value/signature.rs`) mapping a stable per-declaration key to the fully
materialized `Signature` Value; `sub_signature_value` now returns the cached
value on a repeat read instead of rebuilding.

The key is NOT `SubData::id` (unstable across bareword rebuilds, as above).
It is the pointer of the `Arc<CompiledFunction>` / `Arc<CompiledCode>` the
rebuild clones from the registry's own `FunctionDef` — an `Arc::clone`, so
the pointee is the exact same allocation across every rebuild of the same
declaration, and (importantly) it is per-declaration, so it also
distinguishes between different `multi` candidates that share a name (an
earlier name-only key collapsed every candidate's signature onto whichever
was materialized first — caught by `t/signature-gist-invocant-format.t`
before landing). A primed (`.assuming(...)`) sub bypasses the cache entirely:
it clones its `SubData` verbatim (same id, same compiled Arc) and only
mutates `assumed_positional`/`assumed_named` on the clone, so two
differently-primed wrappers of the same declaration would otherwise collide
on one cache entry (caught by `t/assuming-signature-gist.t`).

Pinned by the new `t/signature-parameter-object-identity.t`. All 24
pre-existing `t/*signature*`/`*param*` files touching `.signature` still
pass; full `make test` is green.

## What is still open

A `Parameter` read through a **method lookup** (`ClassName.^find_method(...)`,
`.^lookup(...)`) still has no stable identity — `classhow_lookup_impl` builds
its `Sub` via the AST-body path (`Value::make_sub`, no
`compiled_routine`/`compiled_code`), so it falls back to the unstable
`SubData::id` and never hits the cache. This is not a regression (it behaved
identically, i.e. always rebuilt, before this fix) and is not what this
ticket's own repro exercised, but it is the same underlying gap for a
different lookup path. Filed separately:
`todo/tickets/method-lookup-signature-has-no-stable-identity.md`.
