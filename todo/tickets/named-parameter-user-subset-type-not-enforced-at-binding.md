# A named `:$param`'s declared type constraint is not enforced at binding AT ALL (not just user subsets)

**Scope correction (2026-08-12): this is far bigger than originally filed.**
The original title/repro below suggested "just a user-`subset` gap on named
params, mirror PR #6277's positional fix". Investigation this session found
the real gap is much wider: **named parameters skip type-constraint checking
entirely, for any type** — built-in (`Int`), user class, or user `subset`.
Minimal Cro-independent repro, no subset involved:

```raku
sub f(Int :$x!) { "ok $x" }
say f(x => "not an int");
```

- raku: `Type check failed in binding to parameter '$x'; expected Int but
  got Str ("not an int")`.
- mutsu: prints `ok not an int` — no check at all.

## Root cause

`bind_function_args_values` (`src/runtime/types/binding_signature.rs`) has
one big `for pd in param_defs` loop with separate arms per parameter kind.
The **positional** scalar arm (starting ~line 1472 as of this writing)
builds `value`, then runs a large, self-contained type-check-and-coerce
block (`if let Some(constraint) = &pd.type_constraint { ... }`, ~230 lines,
handles subset resolution via `registry().subsets`, coercion types like
`Str(Numeric:D)`, `:D`/`:U` smiley diagnostics, `&`-sigil Callable-return-type
checking, Num-widening, Associative/Hash coercion — plus more type-adjacent
checks after it: literal-value equality, native-int wrapping, and implicit
Any/Positional/Associative constraints for untyped `$`/`@`/`%` params).

The **named** arm (`else if pd.named || pd.name.starts_with(':')`, a
separate ~300-line block later in the same loop) resolves the matching
`Pair` argument, handles rename aliases (`:min(:$minutes)`), `is rw`/`is
raw` container sharing, and `code_signature`/`where_constraint` checks — but
**never calls into the type-constraint block at all**. It binds the raw
value directly (`self.bind_param_value(&pd.name, bound_value);`) with zero
type validation.

## Why this wasn't caught by PR #6277

That PR (see `session-subset-nominalize-and-typecheck-parameter-fix`
memory) fixed `Parameter.type` *reflection* (nominalizing a user subset's
base type for introspection/`Signature.ACCEPTS`), plus crash sites where
`X::TypeCheck::Binding::Parameter.parameter` was missing. It did not touch
this binding-time gap because the positional path *did* already call
`type_matches_value` (which independently resolves subsets correctly) —
only the named path skips the call altogether.

## Why this is a bigger fix than it looks

The type-check-and-coerce block is not a clean standalone function — it's
inline in the middle of the positional arm, references the arm's local
`value` by mutable reference, and several *other* checks after it (native-int
wrapping, literal equality, implicit Any/Positional/Associative) probably
also need to apply to named params for full parity, once you start pulling
on this thread. A safe fix should:

1. Extract at least the `if let Some(constraint) = &pd.type_constraint {
   ... }` block (~lines 1579–1809) into a standalone method taking
   `(&mut self, pd: &ParamDef, value: Value) -> Result<Value, RuntimeError>`,
   called from both the positional arm and the named arm's "found" case
   (~line 1143, right before `self.bind_param_value`).
2. Verify whether the named arm's *other* value-producing cases (explicit
   `pd.default` expression ~line 1171, and the unsupplied-optional
   type-object fallback ~line 1246) should also route through it — check
   what raku actually does for each before wiring them in (a default
   expression's type mismatch is often a *compile-time* SORRY! in real raku,
   which mutsu does not implement, so it may not need a binding-time check
   at all — verify with `raku -e` before assuming).
3. Run the FULL local parameter-binding test suite before touching anything
   in the positional arm — it is extremely well-covered and this code is
   delicate (COW/copy semantics, rw/raw container sharing, sigilless alias
   handling all sit right next to the block).

This is exactly the kind of "grow the interpreter properly" work
CLAUDE.md's "Refactor boldly" section calls for, but it deserves a
dedicated session's full attention rather than a tail-end addition —
recorded here instead of rushed.

## Effect on Cro

`t/http-router.rakutest` (vendored Cro::HTTP suite) tests 191/192/194/195
("Non-matching (optional) unpack gives 400 error (subset, Str/Int)") expect
a 400 when a route's named `is query` parameter fails its subset
constraint; mutsu returns 404 because the binding never rejects the
mismatched value in the first place (Cro's router only produces the correct
400 by *catching* the `X::TypeCheck::Binding::Parameter` a real bind throws).

To reproduce via Cro: `bash tmp/cro-t.sh t/http-router.rakutest` (see
`handoff-cro-next-steps` session memory for the Cro campaign scaffolding
under `tmp/cro-work/`).
