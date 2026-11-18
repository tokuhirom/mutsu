# An EXPORTHOW declarator's kind comes from its HOW object

A module's `EXPORTHOW::DECLARE` declarator whose metaclass inherits from
`Metamodel::ParametricRoleHOW` now declares a role, as on rakudo:

```raku
# in the module
my class TraitRoleHOW is Metamodel::ParametricRoleHOW { }
my module EXPORTHOW { package DECLARE { constant trait-role = TraitRoleHOW } }

# in the importer
trait-role Greets[$who] { method hi { "hi $who" } }
class Person does Greets["bob"] { }
```

mutsu used to parse every imported declarator like `class` and failed on the
role's signature with "Two terms in a row". Since ADR-17558 the importer's
parse loads the module when it reaches the `use`, so the parser now asks the
loaded unit what each declarator's HOW builds instead of guessing from the
module's source. A declarator the module binds at BEGIN time
(`EXPORTHOW::DECLARE.WHO<gizmo> := GizmoHOW`), which the source does not
spell out, is now imported too. This is the declarator half of ADR-17558
slice 3.
