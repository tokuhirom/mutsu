# Role mixin identity is now stable across repeated evaluations of the same declaration site

ADR-0060 gave a role-mixed value's `.WHAT` a genuine per-composition identity,
keyed by `(base type, role name, role_id, typeargs)` via
`crate::value::types::mixin_composition_key`. A `todo/tickets/` finding
observed that composing the *same anonymous role literal* twice — e.g. by
calling a sub that returns `Foo.new but role :: { ... }` twice — produced two
distinct `.WHAT`s in mutsu, where Rakudo gives them the same one:

```raku
class Foo { has $.x = 1; }
sub mk() { return Foo.new but role :: { has $.tag = "hello" }; }
my $o1 = mk();
my $o2 = mk();
say $o1.WHAT === $o2.WHAT;   # raku: True, mutsu (before this fix): False
```

## Root cause

The ticket's original hypothesis was that the anonymous role's own name/id
(the `<anon|N>` counter) was being re-minted on every runtime evaluation. That
turned out to be already correct — `ANON_ROLE_COUNTER` in
`src/parser/primary/misc/anon_decl.rs` assigns the anon role's *name* once per
parse-time literal, so both evaluations already reported the same
`Foo+{<anon|1>}` from `.^name`.

The real bug was one layer deeper, in `register_role_decl`
(`src/runtime/registration_role.rs`): every time the `RegisterRole` bytecode
op executed — which happens on *every call* to the enclosing sub/block, not
just once — it called `super::next_role_id()` to mint a **fresh** `role_id`
for the `RoleDef`, and stamped it onto the mixin's `overrides` map as
`__mutsu_role_id__{role_name}`. `mixin_composition_key` then included that
fresh id in the composition key it hashes into `.WHAT`'s cache lookup, so two
calls to `mk()` produced two different keys even though they composed the
textually identical role literal.

Re-testing during this investigation also showed the bug was **broader** than
the ticket assumed: it affected *named* roles the same way whenever the `my
role R { ... }` declaration lived inside a repeatedly-invoked sub, not just
anonymous role literals:

```raku
sub mk() { my role R { has $.tag = "x" }; return 1 but R; }
say mk().WHAT === mk().WHAT;   # raku: True, mutsu (before this fix): False
```

## Fix

`role_id` is now minted **once per declaration site, at compile time**,
mirroring how the anonymous-role name counter already works. A new
`CompiledRoleDeclPlan::role_id` field (`src/opcode.rs`) is assigned via
`crate::runtime::next_role_id()` when the plan is built in
`add_role_decl_plan` — which runs once per `role`-declaration AST node,
regardless of how many times the surrounding code later executes at runtime.
`exec_register_role_op` (`src/vm/vm_typedecl_ops.rs`) now threads that
plan-level id through to `register_role_decl`, which stamps it onto the
`RoleDef` instead of calling `next_role_id()` itself on every registration.

This still keeps two textually distinct `my role A { ... }` declarations in
different scopes — even ones that reuse the same short name — as distinct
identities, since each is compiled as its own `CompiledRoleDeclPlan` entry
with its own id. Only *re-registrations of the same plan* (repeated calls)
now share an id.

## Testing

`t/mixin-what-declaration-site-stable.t` (new, passes under both `raku` and
mutsu) covers: the same anon role literal evaluated twice sharing one
`.WHAT`; two different anon role literals NOT sharing one; the same named
role declared inside a repeatedly-called sub sharing one `.WHAT`; two
distinct `my role A {}` declarations (different scopes, same short name) not
sharing one; a role composed via `does` twice sharing one `.WHAT`; and
`.^name` still rendering the anon role as `<anon|N>`.

The whitelisted `roast/S32-exceptions/misc.t` and all 24 whitelisted
`roast/S14-roles/*.t` files (including `roast/6.c/S14-roles/mixin-6c.t`,
`roast/S14-roles/anonymous.t`, `roast/S14-roles/parameterized-type.t`,
`roast/S14-roles/stubs.t`, `roast/S14-roles/mixin-6e.t`) continue to pass.
