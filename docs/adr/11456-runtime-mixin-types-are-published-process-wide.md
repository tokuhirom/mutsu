# ADR-11456: Runtime-created mixin types are published process-wide as recipes

- **Status**: Accepted (implemented)
- **Date**: 2026-10-11
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11456](https://github.com/tokuhirom/mutsu/issues/11456)
- **Related**: ADR-0019 §2 (one canonical type/method registry per interpreter), #12825
  (role-body lexicals are per-thread)

## 1. Context

Every thread runs on its own interpreter. `TypeState::fork_for_thread` gives the child a
copy-on-write snapshot of the parent's declaration registry: the child sees what the parent
declared, and what the child declares does not leak back. For nearly every run-time type that
is harmless, because a type created on a thread is only reachable through values that thread
created.

`$obj does R` breaks that assumption. Like Rakudo, mutsu implements it by composing the type
`C+{R}` and reblessing the object into it in place (`Interpreter::does_rebless_instance`). The
type tag lives in the object's shared `InstanceAttrs` node, so every alias on every thread sees
the new class name at once — but only the thread that ran `does` has `C+{R}` in its registry.
Elsewhere, `$obj ~~ R` answered `False`, `$obj.method-from-R` threw `X::Method::NotFound`, and
the MOP described a type with no roles (#11456).

## 2. Decision

A mixin type is fully described by its **recipe**: the base class and the roles composed onto
it (`C+{A}+{B}` is base `C+{A}` plus role `B`). Composing the same recipe from the same
declarations yields the same type.

1. `TypeState::published_mixins` is one `PublishedMixins` table shared (an `Arc` clone, not a
   snapshot) by an interpreter and every thread forked from it. `ensure_mixin_class` publishes
   each type it creates there.
2. `Interpreter::adopt_published_mixin(name)` composes a published type into the current
   interpreter's registry through the same `ensure_mixin_class` path, adopting a stacked
   type's base first. It is called where an object's class name is resolved and may miss the
   registry: method resolution (`resolve_method_with_owner_impl`) and type matching
   (`type_matches_value_why`).
3. The probe costs one atomic load when nothing was ever published, and one more suffix test
   for a name that cannot be a mixin name (`PublishedMixins::may_hold`); only a mixin-shaped
   name takes the registry and table read locks.

## 3. Rejected alternatives

- **Copy the creating thread's registry entries.** A class is spread over dozens of
  class-keyed registry tables plus compiled methods and non-registry state; a copy that misses
  one table fails silently. Re-composing uses the one path every other mixin already uses.
- **Derive the roles from the class name.** Works for smartmatch only; method dispatch and
  the MOP need the composed class, and names do not round-trip for every role spelling.
- **One process-wide registry.** It would end the per-thread snapshot semantics ADR-0019 and
  the copy-on-write fork rely on — a far larger change than this bug needs.

## 4. Consequences

- A role's body runs once more on each thread that adopts a mixin type, and the lexicals it
  declares are that thread's own. This matches mutsu's existing behaviour for types composed
  before a thread starts (role-body lexicals are per-thread there too), and diverges from
  Rakudo, where the body runs once per composition and its lexicals are shared. Both are
  #12825; once role-body lexicals have one shared home, adoption can bind them instead of
  re-running the body.
- Only `does` on an `Instance` creates shared retagged objects today; parameterised and
  non-instance mixins keep the per-value wrapper, which carries its roles with the value.

## 5. Amendment (2026-10-12, #12825): adoption binds the creator's role-body lexicals

The first consequence in §4 is retired. A class or role body's scalar `my` lexical is now
stored in `package_lexicals` as a `ContainerRef` cell (`share_body_static_scalar`), so every
thread's copy of the table shares one container, as Rakudo's single closure does. A
`MixinRecipe` also carries the composed type's `package_lexicals` entry as the creating
thread left it (`body_lexicals`). An adopting thread marks the recipe's `class:<type>:<role>`
compositions done before it composes the type, so the role bodies do not run again, and then
installs those entries. The cells are the creator's, so both threads read and write the same
`$n`. `@`/`%` statics are recorded as they are, without a new cell. Their node is shared, but
a write that copies an aliased node is not covered by this amendment.
