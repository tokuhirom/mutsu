# A user-exported `postcircumfix:&lt;[ ]&gt;` multi candidate is never dispatched for `@obj[...]` subscript syntax

## Status update: the READ-path dispatch gap is fixed

The core claim below — that `@obj[...]`/`%obj{...}` *read* access never consults
a user-declared `postcircumfix:&lt;[ ]&gt;`/`postcircumfix:&lt;{ }&gt;` multi
candidate for an `Instance` target — is fixed: `exec_index_op_with_positional`
(`src/vm/vm_var_index_ops.rs`) now probes `resolve_function_with_types` for the
op name against `[target, index]` *before* the built-in AT-POS/AT-KEY arms,
mirroring how `prefix:&lt;~&gt;`/`infix:&lt;...&gt;` operator overloads are
checked ahead of their native fallback. Candidate specificity (`Int:D` beating
`Any:D`) and the "no candidate declared at all" fast path (`Registry::functions`
name-existence gate, same one `prefix:&lt;~&gt;` already relies on) both work
as in real `raku` — pinned in `t/user-postcircumfix-index-instance.t`.

## Status update 2 (2026-08-20): assignment path fixed; callable-term gap turned out to be a deep, separate ADR

Re-triaged against current `main`. Item 1 below is now fixed. Item 2 was
re-investigated and found to be bigger and different than originally
described — see ADR-0041 (new). Item 3 remains untouched and unrelated.

1. **FIXED.** The *assignment* path (`@obj[i] = v` / `%obj{k} = v`) now
   consults a user 3-argument `multi sub postcircumfix:<[ ]>(SELF, index,
   value)` / `postcircumfix:<{ }>` candidate before the built-in
   `BIND-POS`/`BIND-KEY`/`ASSIGN-POS`/`ASSIGN-KEY` dispatch, mirroring the
   read-side fix above — confirmed against real `raku` that assignment
   dispatches through a genuinely separate 3-arg multi-sub form (not "read
   via the 2-arg candidate, then STORE into what it returns" as originally
   guessed here; a 2-arg-only candidate makes `raku` itself refuse `@obj[i] =
   v` at compile time with "Calling postcircumfix:<[ ]>(..., Int, Int) will
   never work"). Implemented in
   `exec_index_assign_expr_named_op_inner`
   (`src/vm/vm_var_assign_index_named.rs`), gated on `!is_bind` (a `:=` bind
   is a separate operator, not consulted here). Pinned in
   `t/user-postcircumfix-index-instance.t`. **Not consulted** by the chained
   (`$q<foo>[0] = v`) or generic (`exec_index_assign_generic_op`) assignment
   sites — only the single-level named-variable path — a narrower follow-up
   if a real-world dist needs it.
   - Along the way, found (and filed separately, NOT fixed here) a genuine,
     narrow, pre-existing bug: a *direct* `.ASSIGN-POS(...)` method call on
     an `is Array` subclass **Instance** silently fails to mutate the backing
     storage (plain-array `.ASSIGN-POS` and subscript-syntax `@a[i]=v` on the
     same instance both already work correctly) — see
     `todo/tickets/assign-pos-direct-call-not-mutating-array-subclass-instance.md`.
     This matters because a real assignment-form postcircumfix candidate
     that delegates to `SELF.ASSIGN-POS(...)` (the natural analog of the
     read-side `SELF.AT-POS(...)` idiom) will hit it.
2. **Still open — bigger than originally scoped; see ADR-0041.** Confirmed
   `&postcircumfix:<[ ]>` still cannot be captured as a pre-augmentation
   term, but the actual failure mode is a **stack overflow**, not "fails
   outright": `my constant &old-same = &postcircumfix:<[ ]>;` resolves to the
   module's OWN not-yet-textually-reached `multi sub postcircumfix:<[ ]>`
   (registered early by mutsu's sub-hoist pass), so `old-same` recurses into
   itself forever. Root-caused to a general mismatch between "a sub is
   callable from anywhere in its enclosing scope" (must stay true — this is
   why the hoist pass exists) and "a `&name` bareword reference captured
   inside a `constant`/`BEGIN` must see only what is textually declared
   before it" (currently false — mutsu's hoist pre-registers everything
   before the first statement runs). **This is not specific to postcircumfix
   or even to operators** — the identical bug reproduces with a plain named
   sub shadowed in an inner block, though it fails a different way there
   (`Redeclaration of routine`, since operator names are apparently exempted
   from the redeclaration check that catches the plain-sub case). Full
   analysis, root cause trace, and rejected/considered fix options are in
   **[ADR-0041](../../docs/adr/0041-sub-hoisting-vs-compile-time-name-visibility.md)**
   (Proposed, investigation only — no implementation attempted). A future
   session should read that ADR first, not restart this investigation.
3. The constant-alias `is` trait gap in "What's still broken" below is
   unrelated and still fully open.

Do not close `Array::Rounded`'s row in `dist-test-suite-failures-batch.md`
until items 2-3 above are resolved, in addition to everything already listed
under "What's still broken".

Found while investigating `Array::Rounded` (`todo/tickets/dist-test-suite-failures-batch.md`'s
Un-triaged list). The dist's actual rounding mechanism is NOT the `AT-POS` method override it also
declares (that one only matters for a *direct* `.AT-POS(...)` call, or the Positional protocol paths
that route through it — `:exists`/`:delete`/`:p`/`:k`/`:v` adverbs). The real mechanism is a set of
**exported `multi sub postcircumfix:&lt;[ ]&gt;` candidates** that intercept the subscript *operator*
itself for a non-`Int` index on the class:

```raku
my constant &old-same = &postcircumfix:<[ ]>;
proto sub postcircumfix:<[ ]>($, |) is export {*}
multi sub postcircumfix:<[ ]>(Array::Rounded:D \SELF, Int:D $index) {
    old-same SELF, $index
}
multi sub postcircumfix:<[ ]>(Array::Rounded:D \SELF, Any:D \index) {
    old-same SELF, index.round
}
# ... more candidates for Iterable/Callable/Whatever/List index shapes ...
```

Confirmed against real `raku` (`raku -e 'say 1.5.round'` → `2`; a minimal two-file repro with `use
Mod2; class Baz is Array {}; multi sub postcircumfix:<[ ]>(Baz:D \s, Any:D \i) {...}` was NOT built
here, but the mechanism is a standard, spec'd Raku operator-overload feature — see
`raku-doc/doc/Language/operators.rakudoc` "Adding new operators" / `is export`). This is a genuine,
general capability gap, not specific to this dist: **any module that defines a custom
`postcircumfix:&lt;[ ]&gt;` (or `postcircumfix:&lt;{ }&gt;`, `postfix:&lt;++&gt;`, etc.) multi
candidate scoped to a user class is never consulted when mutsu compiles `@obj[...]`/`%obj{...}`
subscript syntax** — that syntax always lowers straight to the native `Index`/`IndexAssign` opcode
family (`src/vm/vm_var_index_ops.rs`, `src/vm/vm_var_assign_index_named.rs`), which for an
`Instance` target only ever calls the built-in `AT-POS`/`AT-KEY`/etc. protocol methods (added
2026-08-06/07, see `news/2026-08/array-subclass-nextwith-and-num-subscript.md`), never checking the
op-name multi-sub table (`postcircumfix:&lt;[ ]&gt;` et al.) the way a *written-out* call to
`&postcircumfix:<[ ]>(...)` or a custom infix (`$a xxx $b`, fixed generally in
`news/2026-08/user-infix-closure-arg-writeback.md` for a related but distinct gap — writeback, not
dispatch-site — earlier this same day) would.

## Why this is deep, not a quick patch

- The compiler currently treats `@expr[...]` as **syntax**, lowering it directly to an `Index`
  opcode at compile time — it does not go through the generic `Undeclared routine` /
  multi-candidate-resolution machinery that a plain function call does. Making it consult a
  user-declared `postcircumfix:&lt;[ ]&gt;` multi means either:
  1. Compiling `@expr[...]` to a call-shaped op that checks "does any user multi candidate for
     `postcircumfix:&lt;[ ]&gt;` match this (target, index) pair, by MRO/type, before falling back to
     the native `Index` opcode" — a real dispatch-site change touching every subscript compile site
     (positional, associative, multi-dim, autovivifying, lvalue-assignment forms all have their own
     opcodes/handlers per `vm_var_index_ops.rs`, `vm_var_assign_index_named.rs`,
     `builtins_multidim_assign.rs`).
  2. Or, cheaper but narrower: at the `Instance` arms already added for `AT-POS`/`AT-KEY` dispatch,
     ALSO probe for a user `postcircumfix:&lt;[ ]&gt;` multi registered against the instance's class
     before falling to `AT-POS`, and route through it if found. This still needs the multi-candidate
     matcher (parameter type checks including `Int:D` vs `Any:D` specificity ordering) invoked from a
     VM op rather than the normal call-site compiled dispatch — a new, narrower entry point into
     `resolve_all_methods_with_owner`-style candidate resolution, but for *subs*, not methods.
  - Either way this is a **dispatch-site redesign for every subscript op**, not a one-line delegation
    fix like the three `Array::Rounded`-adjacent bugs fixed alongside this ticket.
- Getting candidate *specificity* right matters for correctness: `Int:D $index` must beat `Any:D
  \index` for an actual `Int` index (raku picks the narrower candidate), so a naive "any matching
  multi wins" implementation would silently break the plain-`Int` fast path this dist ALSO relies on
  (`multi sub postcircumfix:<[ ]>(Array::Rounded:D \SELF, Int:D $index) { old-same SELF, $index }`).

## What's confirmed fixed alongside this (do not re-investigate)

Three genuine, general, already-fixed bugs surfaced along the way (verified against real `raku`,
regression-pinned in `t/array-subclass-new-default-ctor.t`):

1. `Foo.new(1,2,3)` for an `is Array` subclass with no user `new` stashed the elements under a stray
   `__array_items` attribute key that no delegation method actually read (they all read
   `__mutsu_array_storage`) — `src/runtime/methods_object_dispatch_new.rs`.
2. `my @a is Foo = ...` (the `is` TRAIT on an `@`-sigil variable) had no implementation at all for a
   user class target — added a new `@`-sigil branch in `exec_apply_var_trait_op`
   (`src/vm/vm_var_trait_ops.rs`), mirroring the existing `%`-sigil tied-hash mechanism.
3. `nextwith`/`nextsame` from inside a single (non-multi, non-wrapped) compiled method — which pushes
   no `method_dispatch_stack` frame — fell straight to the "MRO exhausted, return Nil" fallback
   instead of reaching the native `Array` base behavior on `__mutsu_array_storage`
   (`src/runtime/builtins_dispatch_next.rs`, new `native_array_storage_next_candidate`).
4. A fractional/`Rat` subscript (`@obj[1.5]`) on an `Instance` fell through every match arm straight
   to `Nil` instead of truncating to `Int` and dispatching to `AT-POS`/`AT-KEY` like a plain `Array`
   does (`src/vm/vm_var_index_ops.rs`) — confirmed against raku that subscript syntax ALWAYS truncates
   before `AT-POS` sees the index (a *direct* `.AT-POS(1.5)` call is the only way to see the
   fractional value — this is what `Array::Rounded`'s `AT-POS` override actually depends on, it's the
   `postcircumfix:&lt;[ ]&gt;` multis, not `AT-POS`, that see the raw subscript expression).

## What's still broken

**Also a separate, un-investigated gap**: `my @a is Rounded = ...` where `Rounded` is a `my constant
... is export` aliasing the real class, imported from ANOTHER module (`use Array::Rounded`), does not
resolve — `@a.^name` stays `Array` (fix #2 above only checked the trait name literally against
`registry().classes`/`registry().roles`, which only matches when the `is` trait names the class
directly, e.g. `is Array::Rounded` or a same-file `my constant`). A direct bareword reference to the
same constant elsewhere (`Rounded.new(...)`) resolves correctly via the normal `GetBareWord`
opcode's resolution chain (`src/vm/vm_var_get_ops.rs`) — `exec_apply_var_trait_op` needs the same
resolution for `trait_name`, not just a registry key match. Not fixed here; `exec_apply_var_trait_op`
(`src/vm/vm_var_trait_ops.rs`) has no access to `compiled_fns`/`code`-driven bareword resolution the
way `exec_get_bare_word_op` does, so plumbing this through needs its own small design pass. This is
`Array::Rounded`-idiomatic (many "provides a nicer-named constant for a verbosely-named class" Raku
modules follow this pattern), so likely affects other dists too.

Do not close `Array::Rounded`'s row in `dist-test-suite-failures-batch.md` until BOTH this ticket
(postcircumfix dispatch) and the constant-alias `is` trait gap above are resolved — the dist's own
test suite still fails 16/35 with both outstanding.
