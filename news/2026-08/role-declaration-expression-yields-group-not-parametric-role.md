# A `role` declaration used as an expression now evaluates to the individual parametric role

`(role Zape2 { })` and `(role Zape[::T] { })` reported
`Perl6::Metamodel::ParametricRoleGroupHOW` where Rakudo reports
`Perl6::Metamodel::ParametricRoleHOW`. Fixed together with two sibling divergences
(`metamodel-parametricrolehow-new-type-wrong-how.md`,
`role-instance-how-wrong-metaclass.md`) — all three turned out to be three *different*
defects that shared one underlying cause.

## The shared root cause

`dispatch_how()` decided a value's metaclass purely from a type **name** looked up in
`registry().roles`. A role name in that table meant "role group", full stop — regardless
of whether the value asking was the group's own type object, one individual declaration,
or an instance made from the punned class. mutsu had no representation-level distinction
between the three meta-objects Rakudo keeps:

* **`ParametricRoleGroupHOW`** — the installed *name* `R`, dispatching across every
  same-named candidate. mutsu already got this right.
* **`ParametricRoleHOW`** — one individual `role` declaration: the value of the
  declaration *expression*, what `.^candidates` hands out, and what
  `Metamodel::ParametricRoleHOW.new_type` produces.
* **`ClassHOW`** — an ordinary class, including the one Rakudo synthesizes when you call
  `.new` on a bare role.

## The fix for this ticket

An individual candidate now gets its own type object: a **declaration-site key** built
from the group name, a `\u{0}` separator, and the declaration's `role_id`
(`src/runtime/types/role_candidate.rs`). This deliberately reuses the convention
[ADR-0047](../../docs/adr/0047-type-identity-is-a-declaration-site-not-a-registry-name.md)
P1 established for lexical class site keys, so `value::display::user_facing_type_name`
already strips it — `.^name`, `.gist`, `.raku` and error messages keep showing the bare
`R` with no new machinery. `RoleGroupToCandidate`, a new zero-operand opcode emitted right
after a `role` declaration compiled in expression position, narrows the bareword lookup
from the group to the candidate.

The key is produced *only* for the declaration expression's value. Every consumer of a
role type object — `but`, `does`, role composition, `extract_role_application`, method
punning, parametric-default materialization — normalises it straight back to the group, so
composition markers, `.^roles` and `~~` never see a site key. `has_role`/`get_role_def`
resolve it, and `~~` against the group is true (`my $r = (role R { }); $r ~~ R`), matching
Rakudo. The distinction exists to answer `.HOW`, which is the whole point.

Because the candidate representation is now shared with `.^candidates`,
`R.^candidates[0].HOW` reports `ParametricRoleHOW` too (it used to report the group HOW).

Pinned by `t/metamodel-role-how-taxonomy.t`, which passes identically under `raku` and
`mutsu`.
