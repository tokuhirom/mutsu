# A `but`/`does` mixin ignores a role group's parameterised candidate

Found 2026-09-06 while writing the pin for
`news/2026-09/role-mixin-init-value-is-not-a-type-parameter.md`; a name
collision inside that test surfaced it. Pre-existing — it reproduces with a
`main` build from before that change, whose diff does not touch candidate
selection.

## Repro

```raku
role Z { has Int $.n }
role Z[::T] { has $.a = T }

say (1 but Z[Str]).a;
# raku:  (Str)
# mutsu: No such method 'a' for invocant of type 'Int'
```

The unparameterised member of the group still works:
`(1 but Z).n` is `(Int)` in both.

## Narrowed

Declaring the same role name twice with different parameter lists forms a role
**group** in raku, and `Z[Str]` selects the parameterised candidate. mutsu's
mixin path composes from `registry().roles.get(role_name)`, which holds *one*
`RoleDef` per name — so it composes whichever candidate was registered last for
that name (here the unparameterised `Z`, whose only attribute is `$.n`), and the
parameterised candidate's `$.a` never exists.

The registry does model groups elsewhere: `role_candidates` is what
`role_default_type_param_bindings` and `ensure_parametric_role_pun_class` (both
in `src/runtime/types/roles.rs`) consult, and `resolve_role_candidate` is what
the class-header path (`class C does Z[Str]`) uses. The **mixin** path is the
one that skips them.

## The class-header path already gets this right — measured

```raku
role Z { has Int $.n }
role Z[::T] { has $.a = T }
class C does Z[Str] { }
say C.new.a;   # both: (Str)
```

So the fix is to route `compose_role_on_value`'s `role` lookup through the same
candidate resolution the class path uses, not to write a second selector.

## Where to look

`Interpreter::compose_role_on_value` (`src/runtime/types/roles.rs`) — the
`self.registry().roles.get(role_name).cloned()` at the top, whose result drives
both the attribute loop and the single-public-attribute check — against
`resolve_role_candidate` / `Registry::role_candidates`.

## Neighbourhood to check when fixing

Two parameterised candidates that differ in arity (`Z[::T]` and `Z[::T, ::U]`);
a candidate selected by a `where` constraint on the parameter; `.^name` for each
(the composed name must show the candidate's own arguments); `.does(Z[Str])`
against a value composed with `Z[Int]`; and the role-punning path
(`Z[Str].new`), which already goes through `ensure_parametric_role_pun_class`
and must keep working.
