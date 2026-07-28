# A punned parameterised role is now a real composed class

`R[Int].new` used to build an ad-hoc mixin instance directly in `dispatch_new`:
a `Mixin(empty-instance, {__mutsu_attr__*, __mutsu_role__*})` assembled from the
role's attribute list. That representation had no composition behind it, so the
pun never ran the role's `BUILD`, never evaluated the role body's deferred
statements against the type parameter, and reported the bare role name:

```raku
role R2[::T] {
    has @!cache handles <AT-POS>;
    my $stride = nativesizeof(T);
    submethod BUILD() { @!cache := Array[T].new(:shape(2)); @!cache[0] = T.new }
}
say R2[Int].new[0].^name;   # raku: Int      mutsu: Nil
say R2[Int].new.^name;      # raku: R2[Int]  mutsu: R2
```

The same shape written as `class W does R2[Int] { }` already worked end to end,
because composition is what binds the type parameters, runs
`RoleDef::deferred_body_stmts`, and pulls the role's `BUILD` into the class. The
pun now takes that same path: `ensure_parametric_role_pun_class` registers a
class named `R2[Int]` whose single parent is `does R2[Int]`, and `.new`
dispatches to it. A parameterised role body's `my` is therefore evaluated once
per composition with `T` bound — previously it was never evaluated at all, and a
role-body `my` read whatever a *previous* package body had left in the
like-named slot.

Making the pun a real class needed four supporting fixes, each a general bug
that the mixin representation had been masking.

**Self-named `does` parents.** `register_class_decl` already dropped a
`does`-role sharing the class's own name from the C3 inheritance parents
(`class Iterator does Iterator`), but it compared the parent with its type
arguments stripped against the class name with them intact, so `R[Int] does
R[Int]` slipped through and died with "C3 MRO cycle detected at R[Int]".

**Smart-matching a real instance against a parameterisation.** An instance
carries no type-argument markers; which parameterisation it satisfies is
recorded on the class that composed the role (`class_composed_roles` keeps the
parameterised spelling). Both `type_matches_value` and the `(_, ParametricRole)`
smart-match arm now consult it, which also fixes the long-standing case that has
nothing to do with punning: `class W does R[Int] { }; W.new ~~ R[Int]` was False.
A *value* type parameter (`role R[$n]`, composed as `R[42]`) has no type object
to subtype-check, so those arguments are compared by spelling.

**Built-in roles in the composed-role walk.** The instance-side transitive walk
over composed roles was gated on the constraint being a *user-declared* role and
only pushed user-declared parents. A role may compose a built-in one
(`role Measured does Real`), and `Real` is not in the user role registry, so
every instance of a class composing such a role failed `~~ Real`. The walk is
now ungated and pushes parents unconditionally, matching the `.does`
introspection walk it mirrors.

**Container attribute delegation.** `forward_resolved_delegation` passed an
`@`/`%` attribute delegate to the target method *by value*, so a mutating target
updated a copy: `class C { has @!c handles <AT-POS ASSIGN-POS> }` silently
dropped `$c[0] = 5`. It only ever appeared to work on a punned role, whose
attributes lived in mixin markers. Container delegates now go through the mut
dispatch path against a temp binding, and the updated container is folded back
into the attribute map. That in turn surfaced a missing interpreter-side
`ASSIGN-KEY` for plain hashes — the VM had one, the interpreter dispatch did
not, so a `handles <AT-KEY ASSIGN-KEY>` delegation died with "No such method
'ASSIGN-KEY' for invocant of type 'Hash'".

Two consequences are visible in behaviour that was previously wrong in mutsu's
favour. `MyRat[Int,Int].new(3, 10)` on a role that declares no `new` now
correctly throws `Default constructor ... only takes named arguments`, exactly
as Rakudo does — the ad-hoc mixin path had been assigning positional arguments
to attributes by index, and `t/role-attr-shadows-builtin-method.t` had that
non-conformance baked in. And `to-json`'s Rational special case, keyed on the
mixin markers, gained an instance arm for the punned class.

Pins: `t/parametric-role-pun-composes.t`, `t/handles-container-attr-writeback.t`.

This was [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
P1's remaining blocker for `NativeHelpers::CStruct`'s `LinearArray`, which now
allocates, computes its element stride from the role-body `my int $sol =
nativesizeof(T)`, indexes, and nativecasts. One pre-existing bug still stands
between it and a working `LinearArray` — see
`todo/tickets/subscript-attr-assign-clobbers-object-var.md`.
