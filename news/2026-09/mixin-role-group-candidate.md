# A `but`/`does` mixin now selects the right role-group candidate

Declaring one role name twice with different parameter lists forms a role
**group** in raku, and `Z[Str]` selects the parameterised member. The mixin
path did not:

```raku
role Z { has Int $.n }
role Z[::T] { has $.a = T }

say (1 but Z[Str]).a;
# raku:  (Str)
# mutsu: No such method 'a' for invocant of type 'Int'   (before)
```

`(1 but Z).n` — the unparameterised member — worked in both, and so did the
class-header spelling `class C does Z[Str]`.

## Root cause

`registry().roles` holds exactly **one** `RoleDef` per name: whichever
candidate registered last. The class-header path never reads it directly — it
goes through `resolve_role_candidate_with_args`, which consults
`Registry::role_candidates` and picks the member whose parameters bind the
supplied arguments. `Interpreter::compose_role_on_value` used the bare lookup,
so a mixin composed the *last-registered* candidate (here the unparameterised
`Z`, whose only attribute is `$.n`) and the parameterised candidate's `$.a`
never existed.

## The fix

`compose_role_on_value` routes its role lookup through the same resolution the
class path uses, for the bracketed spelling with arguments. The bare lookup
stays as the fallback for everything the resolver declines — an unknown name, a
builtin role, or an initialiser argument (`1 but I(42)`), which is not a type
argument and still reaches the single-public-attribute path.

Two things also had to come from the *selected* candidate rather than from the
name:

- **Its own parameter names.** `role W[::T]` and `role W[::T, ::U]` bind
  different lists, and `role_type_params` records only one of them per name —
  which is why `(1 but W[Str, Int]).w` answered `1:Str`, binding the wrong
  candidate's single parameter.
- **Its `role_id`,** so `(1 but Z[Str]).WHAT =:= (1 but Z).WHAT` is `False`
  while two compositions of the same candidate share a type.

## Measured against `raku`, all matching

The repro; `.^name` for each composition (`Int+{Z[Str]}` vs `Int+{Z}`);
`.does(Z[Str])` and `.does(Z[Int])` against a value composed with `Z[Str]`;
candidates differing in arity (`W[::T]` / `W[::T, ::U]`) and their composed
names; a candidate selected by a `where` constraint on the parameter
(`P[$n where * > 3]`); composed-type identity across two mixins; the
initialiser spelling; the class-header path; and the role-punning path
(`Z[Str].new`), which already went through `ensure_parametric_role_pun_class`
and is unchanged.

(`role V[::T where Int]` is not part of the matrix: rakudo itself refuses to
compile it — "Cannot do non-typename cases of type_constraint yet".)

## Testing

New `t/mixin-role-group-candidate.t` (16 assertions), which passes unchanged
under rakudo.
