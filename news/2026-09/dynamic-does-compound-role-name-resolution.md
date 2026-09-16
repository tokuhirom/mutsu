# Dynamic `does` now resolves a lexically-scoped compound-named role by its written name

A dynamic (runtime) `does` mixin naming a role by a compound, dot-qualified
short name — `$obj does Formatted::Named(:x(1))`, where `my role
Formatted::Named { ... }` was declared earlier in the same file — used to
misparse the dotted name as a package-qualified sub call: `Formatted::Named`
was read as "call `&Named` inside package `Formatted`" instead of "the
lexically-scoped role `Formatted::Named`", dying with `Could not find symbol
'&Named' in 'GLOBAL::Formatted'`.

The role itself was already registered correctly: `my role Formatted::Named`
declared inside `unit module Foo` registers as `Foo::Formatted::Named`
(`exec_register_role_op`), exactly like the equivalent `my class` case. The
gap was in `resolve_bare_type_name` (the call-position resolver `does`/`but`
share with a bare type name invoked with arguments): it refused to walk the
enclosing-package chain for any name already containing `::`, on the
assumption that a qualified name is already fully resolved. That assumption
holds for a name that already resolves as written, but not for a role's own
*relative* compound name, which is only reachable through the enclosing
package chain — the same chain a single-segment short name already walks.

Fixed by letting `resolve_bare_type_name` walk that chain for a compound name
too, but only when the name does not already resolve on its own — so an
already-qualified or otherwise-resolvable compound name is untouched. This
reuses the existing per-call, stack-derived `bare_name_packages()` chain
(scoped to the actually-executing frame's lexical package) rather than
writing any new alias into environment state, so it cannot leak a role's
short name across unrelated modules that happen to share a process (an
earlier attempt that aliased the name directly into `env` at declaration time
did exactly that, and regressed
`t/oo/role/my-role-compound-name-qualification.t`).

This was the second of two bugs blocking `App::Prove6` (via Getopt::Long)
from loading end-to-end, after #8560's `trait_mod:<is>` import fix and
alongside #8577 (still open: a dynamic `does` with a named-arg-parametrized
role attribute loses the attribute's value).

Regression test: `t/oo/role/does-compound-role-name-dynamic-mixin.t`, with
fixture `t/lib/DynamicDoesCompoundRole.rakumod` mirroring Getopt::Long's exact
shape (a `my role` with a dotted name, mixed into a value dynamically from a
sibling `sub` in the same module).

Closes #8578.
