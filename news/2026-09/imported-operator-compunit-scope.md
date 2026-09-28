# An imported operator no longer leaks into other modules' arithmetic

An `is export` operator candidate that the script imported was also a
candidate for the same operator inside every *other* module's routines
(issue #9944). Rakudo resolves operators lexically, so a module that never
imported the candidate sees only the core operator. In the Bitcoin
distribution, every integer `*` in `FiniteField`'s extended-Euclid loop ran
`secp256k1`'s `where 1 < $n < 2**256` clause. One point operation took 0.8 s,
and `t/basics.t` timed out.

Operator imports are now bound to the compilation unit that performed them
([ADR-0131](../../docs/adr/0131-imported-operator-candidates-are-scoped-to-the-importing-compunit.md)).
The name-level gate `user_declared_infix_ops` records the importing unit
instead of "visible everywhere". A new `operator_import_units` table filters
the candidate walk, so an imported family is visible only to its declaring
unit and its importers (`runtime/operator_scope.rs`).

Three class-body and role-body gaps were fixed along the way:

- `class K { use Op; method m { 2 ** 3 } }` now reaches `Op`'s operator. The
  method body shares the unit's constant-folding state, so the literal is no
  longer folded against the core `**`. The unit module's other routines no
  longer see the operator, because the import lands in `K`.
- `unit class C; use Op; method m { $x ** 3 }` sees the operator when a
  method-dispatch path did not switch `current_unit`. The executing frame's
  own file is now also consulted.
- A role body's `use` imports into the role's compunit, not the composer's.

Pinned by `t/modules/compunit/operator-import-is-compunit-scoped.t`.
