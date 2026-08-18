# A method's `Signature`/`Parameter` now has stable identity

```raku
role Q { }
class C { method m(:$mp) { } }
my $p = C.^find_method('m').signature.params[1];   # 0 is the invocant
$p does Q;
say C.^find_method('m').signature.params[1] ~~ Q;  # raku: True   mutsu: False
```

Split off from `todo/tickets/parameter-objects-have-no-stable-identity.md`
(resolved for plain subs and closures — see
`news/2026-08/parameter-object-identity-cached.md`) as the one lookup path
that fix did not cover.

## Root cause

`sub_signature_value`'s cache keys on the `Arc<CompiledFunction>`/
`Arc<CompiledCode>` a rebuilt `SubData` clones from the registry's own
`FunctionDef`. `classhow_lookup_impl` (`.^lookup`/`.^find_method`) builds
its `Method` Instance — and its `Signature` — straight from the AST
`MethodDef` stored in the registry, via `Value::make_sub(...)`, with
neither field set. Every `.^find_method(...)` call therefore fell back to
the cache key's `SubData::id` fallback, which is itself fresh on every
call, so the cache never hit.

## Fix

Added `SubSignatureKey::Method(String)`, keyed on
`"{owner}::{name}#{candidate_idx}"` — the same `(owner, name)` pair already
computed for the doc-comment (`.WHY`) lookup, extended with the candidate
index so different `multi` candidates sharing a name (and same-named
methods on different classes) don't collide on one cache entry.
`make_method_object_with_owner_ex` now checks this cache before building a
fresh `Signature`, gated the same way the existing `__mutsu_lookup_
candidate_idx`/`package`/`__mutsu_method_callable` attrs already are (a
real candidate, not the synthetic multi dispatcher).

## Tests

`t/method-lookup-signature-parameter-identity.t` (new, the method-lookup
twin of `t/signature-parameter-object-identity.t`) — mixin persistence
across repeated `.^find_method(...).signature` reads, object identity
(`===`) across reads, and no cross-contamination between different methods
on the same class or the same method name on different classes. Also
manually verified multi-candidate signatures stay correctly distinguished
(`.candidates[0].signature` vs `.candidates[1].signature`).

PR [#6612](https://github.com/tokuhirom/mutsu/pull/6612).
