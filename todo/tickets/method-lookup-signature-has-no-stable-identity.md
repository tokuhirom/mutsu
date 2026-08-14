# A method's `Signature`/`Parameter` still has no stable identity

Split off from `todo/tickets/parameter-objects-have-no-stable-identity.md`
(now resolved for plain subs and closures — see
`news/2026-08/parameter-object-identity-cached.md`) as the one lookup path
that fix did not cover.

```raku
role Q { }
class C { method m(:$mp) { } }
my $p = C.^find_method('m').signature.params[1];   # 0 is the invocant
$p does Q;
say C.^find_method('m').signature.params[1] ~~ Q;  # raku: True   mutsu: False
```

Not a regression — this never worked (verified against a pre-fix build too),
and `Interpreter::sub_signature_value`'s new cache
(`src/runtime/methods_signature_candidates.rs`) is keyed primarily on the
`Arc<CompiledFunction>`/`Arc<CompiledCode>` pointer a rebuilt `SubData`
clones from the registry's `FunctionDef`. `classhow_lookup_impl`
(`src/runtime/methods_classhow_lookup.rs`) builds the `Sub` Value for a
method via `Value::make_sub(...)` on the AST body directly, with neither
field set, so every `.^find_method(...)` call falls back to the cache key's
`SubData::id` fallback — which is itself fresh on every call (a new
`Value::make_sub` invocation), so the cache never hits.

## Sketch of a fix

The same identity problem as the sub case, needing the same kind of stable
key, but `classhow_lookup_impl` has no `Arc` to reuse. Candidates:

- Give a method declaration its own registry-level stable identity (the
  `ClassDef`'s stored `MethodDef`/`FunctionDef` already lives at a fixed
  address inside the registry — could key on that, analogous to the sub
  case's `Arc<CompiledFunction>`, if `classhow_lookup_impl` is changed to
  clone through a real `Arc` there instead of the raw AST body it currently
  copies).
- Or a coarser but simpler key: `(owner_str, method_name, candidate index)` —
  keyed like the sub case's discarded name-only attempt, but this time actual
  multi-candidate handling needs auditing first (does `.^find_method` even
  distinguish candidates today, or only ever return `defs.first()`? — read
  `classhow_lookup_impl` fully before assuming the shape).

Not urgent: introspecting a `Method`'s own `Parameter` objects for mixin
identity is a narrow use case (Cro::HTTP::Router's custom-trait need is
already covered by the earlier trait-replay fix,
`news/2026-08/param-trait-mixin-persists.md`).
