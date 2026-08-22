# ADR-0042 slice 1: a container's type constraint is read from the container

`Interpreter::var_type_constraints` is a `HashMap<String, String>` keyed by BARE
variable name and never frame-scoped. A `my Int @a` anywhere in the process
registered `"@a" -> "Int"` globally, and any later `@a` — a different variable,
in a different frame, in a different file — was type-checked against it. Two
earlier fixes (2026-08-13) had scoped typed *scalars* in routine bodies and in
genuine `{ ... }` blocks, but containers were explicitly excluded from both:
`Compiler::emit_set_var_type` skipped the scoped opcode by sigil for `@` and `%`
precisely because the push/subscript fast paths read element metadata through
the global map.

The deep ticket sized the container half as the architectural one. That was
backwards, and ADR-0042 corrected it. `ArrayData` and `HashData` already carry
`value_type` / `key_type` / `declared_type`, and the decisive measurement is an
alias probe: binding a *differently-named* alias (`my @x := @a`) and pushing a
bad element through it still enforced in 8 of 8 container shapes. Enforcement
reached through a different name cannot be coming from a name-keyed map — it was
already coming from the container. So for containers the map was not the
mechanism at all; it was a redundant second source of truth contributing only
false positives. The container-first accessor `element_constraint_for` even
existed already, with a doc comment stating the thesis. The thirteen
`var_type_constraint_fast` call sites simply did not use it.

Slice 1 broke the circular dependency at the *read* sites rather than the
declaration sites. The ten container mutation chokepoints (push, the shared- and
fast-hash/array element-assign bailouts, `:delete`, the whole-`%h` Mix-trait RO
check, QuantHash coercion on hash store, the `__ANON_STATE__` guard) now consult
`element_constraint_for` / `container_type_metadata` instead of the name map.
`state` containers are tagged with their `ContainerTypeInfo` at declaration,
closing the one measured shape whose constraint lived only in the name map.
`@`/`%` were dropped from the sigil exclusion so a typed container declared in a
routine or block registers env-scoped like a scalar does (`&` stays excluded),
and `BlockLocalScope`'s exit cleanup now strips the `__mutsu_type::` /
`__mutsu_hash_key_type::` metadata keys, mirroring what `BlockScope` already did.

Two regressions surfaced during the work and were fixed in the same change.
`set_var_type_constraint_routine_scoped` had only ever registered the value-type
env entry, never the key-type twin — harmless until step 3 routed a key-only
object hash (`my %h{Int}`, empty `value_type`) through that scoped path for the
first time, which silently dropped key-type enforcement for exactly that shape.
Relatedly, three hash-element bailout checks had to route through
`container_type_metadata` rather than `element_constraint_for`, because the
latter filters out an empty `value_type` — which is precisely what a key-only
object hash has, so it alone would have reintroduced the same gap at the read
side. `var_hash_key_constraint_fast` lost its last caller and was removed.

The result is `raku`-oracled green: the ADR's container matrix matches `raku` in
all 7 shapes (routine / block / `if` / `while` / `for` bodies, plus `my Int %h`
and `my %h{Int}`), the `if`/`unless`/`else` scalar rows go green as a side
effect of the `BlockLocalScope` cleanup, the alias probe matches 8/8 including
`state`, and there were zero regressions across the 62 pre-existing `t/*typed*`
files and the `S02-types` / `S09-typed-arrays` roast whitelist.

Pinned by `t/typed-constraint-scope-matrix.t` and
`t/state-typed-container-alias.t`. A separate pre-existing "outer-first shadow"
bug surfaced by this work — a typed declaration that *shadows* an existing outer
binding leaking its constraint onto that binding — was fixed two days later and
is written up in `news/2026-08/typed-declaration-shadow-scope-leak.md`.

ADR-0042 slices 2 (a scalar cell carries its `of`) and 3 (delete the map and its
workarounds) remain open; `todo/deep/bare-name-type-constraint-store-is-scope-blind.md`
stays open for them.
