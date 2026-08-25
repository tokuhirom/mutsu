# `Metamodel::<X>HOW.new_type(...)` now yields a type whose `.HOW` is that metaclass

`Metamodel::ParametricRoleHOW.new_type(name => "zape", group => "Zape")` returned a type
object reporting `Perl6::Metamodel::ClassHOW`. The ticket's hypothesis was right and the
divergence was broader than the one metaclass it was filed against: **every**
`Metamodel::*HOW.new_type` collapsed to `ClassHOW`. Measured against `raku`:

| call | raku | mutsu (before) |
| --- | --- | --- |
| `Metamodel::PackageHOW.new_type(...)` | `PackageHOW` | `ClassHOW` |
| `Metamodel::ModuleHOW.new_type(...)` | `ModuleHOW` | `ClassHOW` |
| `Metamodel::GrammarHOW.new_type(...)` | `GrammarHOW` | `ClassHOW` |
| `Metamodel::ParametricRoleHOW.new_type(...)` | `ParametricRoleHOW` | `ClassHOW` |
| `Metamodel::ParametricRoleGroupHOW.new_type(...)` | `ParametricRoleGroupHOW` | `ClassHOW` |

## Root cause

`new_type` (`src/runtime/methods_instance_ops.rs`) is a single generic handler gated on
the invocant being a `Metamodel::*` package. It read the `name` argument, registered an
empty class definition under it so `.new` would work, and returned a bare
`Value::package(name)` — discarding *which* metaclass the call was made on entirely. The
name then reached `dispatch_how()`, which found a registered class and answered
`ClassHOW`, as it does for any other class name.

## Fix

`new_type` now records the invoking metaclass in a new registry table
`declared_native_how` (type name → `Perl6::Metamodel::<X>HOW`), and `dispatch_how()`
consults it before its name-kind decision ladder. This is deliberately separate from
`class_how_values`, which carries *user* HOW instances installed by the EXPORTHOW/DECLARE
protocol — mixing a native metamodel name into that table would have put it in front of
`declare_drive_how_protocol`'s user-method probing.

The metaclass name is derived from the invocant (`Metamodel::X` → `Perl6::Metamodel::X`),
so a metaclass mutsu grows later works without another edit — no per-HOW special-casing.

Fixed alongside `role-declaration-expression-yields-group-not-parametric-role.md` and
`role-instance-how-wrong-metaclass.md`; pinned by `t/metamodel-role-how-taxonomy.t`.
