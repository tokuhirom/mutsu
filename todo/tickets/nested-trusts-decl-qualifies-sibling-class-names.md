# A `trusts` declaration inside a nested class body package-qualifies every other class in the file

Found while adding `t/metamodel-introspection.t` (the `Metamodel::Trusting` /
`Metamodel::Versioning` work). Reproduces on plain `main` — this is NOT a regression
from that change; it was verified by building an unmodified `main` and running the
repro below.

## Repro

```raku
class Plain { }
say Plain.^name;          # mutsu: Outer::Plain    raku: Plain
class Outer {
    our class Inner {
        trusts Outer;
        method !secret() { 'from Inner' }
    }
    method poke() { Inner.new()!Inner::secret() }
}
say Plain.^name;          # mutsu: Outer::Plain    raku: Plain
say Outer.poke;           # from Inner (correct)
```

`Plain` is declared *before* `Outer` and reports `Outer::Plain` from the very first
`say` — so the mis-qualification is not an ordering effect of the runtime walk, and it
affects the whole compilation unit.

## Trigger is a conjunction of three things

All three are required; drop any one and `Plain.^name` is correct again:

1. a class nested inside another class body (`my class` and `our class` behave the same),
2. a `trusts` declaration in that nested class body, and
3. a *qualified* private call (`$o!Inner::secret()`) somewhere in the outer class.

Replacing (3) with an unqualified `self!secret()` inside the nested class, or removing
(2), makes the whole file report unqualified names again. A top-level (non-nested)
`trusts` + qualified private call is also fine.

## Why it matters beyond cosmetics

The mis-qualified name is the key everything else is stored under, so it silently
breaks unrelated metadata lookups in the same file. Concretely, `class D:ver<1.2.3> { }`
records its version in `type_metadata["D"]`, but `D.^ver` then looks it up under
`"Outer::D"` and answers `(Mu)`. That is how the bug was found: four `:ver`/`:auth`/
`:api` assertions in `t/metamodel-introspection.t` failed purely because a `trusts`
case lived at the bottom of the same file. The nested-`trusts` cases were split into
`t/trusts-nested-lexical-class.t` to keep the two apart; merge them back once this is
fixed.

## Starting points

- The name is already wrong at the first statement, so suspect the compile-side
  qualification (`Compiler::current_package` / `qualified_class_decl_name`,
  `src/compiler/mod.rs`, `src/compiler/decl_plan.rs`) rather than the runtime
  registration walk — something appears to leave `current_package` set to the
  enclosing class for the whole unit.
- `Stmt::TrustsDecl` lowering lives in `src/opcode.rs` (`add_class_decl_plan`, the
  `let trusts = body.iter().filter_map(...)` block) and
  `src/runtime/registration_class_validate.rs` (`publish_class_shell`).
- The qualified-private-call validation that has to be present for the trigger is
  `validate_private_access_in_expr` in `src/runtime/registration.rs`.
