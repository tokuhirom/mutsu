# `does PositionalBindFailover` composes, and one core-role oracle now answers for everyone

`class Foo does PositionalBindFailover { }` died with
`X::InvalidType: Invalid typename 'PositionalBindFailover'`. So did
`class Foo does Sequence { }` and `class Foo does QuantHash { }`; `raku` composes all
three without complaint.

## Root cause — the fourth, fifth and sixth "is this a role?" oracle

The immediate cause was that `validate_class_parents`
(`src/runtime/registration_class_validate.rs`) accepts a `does`/`is` parent only when it
is a registered class, a registered role, a registered enum, or a member of the
hardcoded `BUILTIN_PARENT_TYPES` list — and `PositionalBindFailover` is none of those.
Sibling core roles like `Positional` and `Associative` work purely because they happen
to appear in `BUILTIN_PARENT_TYPES`, which is a list of legal *inheritance parents*, not
a model of what a role is.

The real finding is that PR #6989's unified core-role oracle, `BUILTIN_ROLE_NAMES`
(`src/runtime/types/type_registry.rs`), was not consulted here at all — and that three
further sites kept their own private copies of "which names are core roles":

- `registration_class_validate.rs` / `registration_class_compose.rs`, via
  `BUILTIN_PARENT_TYPES`;
- `methods_introspect.rs`'s `.HOW` reporting, via an inline `matches!` list that had
  drifted — it omitted `Blob`, `Buf`, `Sequence`, `QuantHash` and `Scheduler`, so all
  five reported `Perl6::Metamodel::ClassHOW` where `raku` reports
  `Perl6::Metamodel::ParametricRoleGroupHOW`;
- `vm_value_helpers.rs`'s `is_builtin_type`, which decides whether a bareword resolves
  to a type object at all — so `Sequence` and `PositionalBindFailover`, having no
  registry entry of any kind, were plain strings rather than type objects.

## Fix

`BUILTIN_ROLE_NAMES` was completed to the full core-role set (adding `Iterator`,
`PredictiveIterator`, `Rational`, `QuantHash`, `Dateish`, `Scheduler`, `Sequence`,
`PositionalBindFailover`), and every one of those four sites now consults it instead of
its own list. Names that ALSO have a real `RoleDef` in the registry are listed anyway,
deliberately: every consumer ORs the two sources, so the constant can be read as "the
complete set of core role names" without a caller having to know which half of the model
any given one lives in.

`vm_mixin_does_ops.rs`'s private `is_role_type_name` helper was promoted to
`Interpreter::is_role_type_name` (`has_role(name) || is_builtin_role_name(name)`) and is
now the shared predicate for "does this name denote a role type object", used by `but`'s
type-object error path, the type-pretense check, and `.^pretending_to_be`.

## Scope

This covers recognizing the type name so composition succeeds, plus the `.HOW`/bareword
consequences above. The deeper `PositionalBindFailover` runtime behaviour — an object
that composes it and defines `.iterator` having that iterator consulted by positional
binding and subscripting — remains part of the already-**Deferred** "custom
`does Iterable`/`does Iterator` protocol" cluster in `docs/doc-diff-backlog.md`.

Pinned by `t/role-composition-gaps.t`.
