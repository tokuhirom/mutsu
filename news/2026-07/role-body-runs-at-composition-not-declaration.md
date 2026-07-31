# A role body runs at composition, not at its declaration

A role's non-declaration body statements used to run when the role was
*declared*, and then again when a class composed it — so this printed `BODY`
twice where Rakudo prints it once:

```raku
role R { say "BODY" }; class C does R { }
```

Rakudo never runs a role body at the declaration. It runs it once per
**composition**: a `does` on a class or grammar, a pun (`R.new`), or a
value-level mixin (`$x but R` / `$x does R`). `role R { say "BODY" }` on its own
prints nothing at all.

mutsu now does the same. The declaration-time run in `vm_typedecl_ops.rs` is
gone, and the three composition paths each run the body:

- `does` composition on a class or grammar already did
  (`registration_class_decl.rs`). The `also does R` form inside a class body is
  a separate arm that did not; it does now.
- Punning now does, in `ensure_role_punned_to_class`. The `classes` lookup at
  the top of that function is not a sufficient memo — a construction path that
  puns a role only to build one instance drops the pun class again afterwards —
  so the run is memoised on a new `Registry::composed_role_bodies` set.
- A value mixin now does, in `compose_role_on_value`. A *parameterised* role
  goes through `ensure_parametric_role_pun_class` so the body sees its type
  parameters bound; a plain one only needs the body run, since punning it to a
  class here would change what `R.HOW` reports.

Composing a role composes the roles *it* composes, so all three paths also run
the ancestor bodies, nearest first — the order Rakudo runs them in:

```raku
role GP { say 'GP' }
role P does GP { say 'P' }
class K does P { }        # P, then GP
```

Their methods already transited into the consumer; only the bodies were
missing, and the declaration-time run had been hiding that.

This closes the last of the three paths noted in the role-body-guard work: a
guard in a parameterised role body now rejects a bad parameterisation on the
mixin path too, with the same `X::Role::Instantiation` Rakudo raises.

```raku
class Ordinary { has $.x }
role Guarded[::T] { die "Need a CStruct" unless T.REPR eq 'CStruct' }
my $o = 5 but Guarded[Ordinary];   # now dies, as in Rakudo
```

## Three general bugs the timing change exposed

Removing the declaration-time run turned up three unrelated defects that it had
been masking.

**`grammar G does R` composed nothing.** The grammar declarators (both the
braced form in `grammar_module.rs` and `unit grammar` in `package_decl.rs`)
recorded a `does` role in `does_parents` only. The role-composition loop in
`register_class_decl` walks `parents` and consults `does_parents` merely to tell
composition from punning, so a grammar's roles were dropped entirely — `grammar
G does R { }` could not call `R`'s methods. Class declarations always pushed the
role into both lists; grammars now do too, and they also accept a parameterised
`does R[T]` instead of skipping the bracket suffix.

**`unit grammar G does R;` did not inherit `Grammar`.** The same code decided
the implicit `Grammar` parent from "`parents` is empty", which a `does` role had
already made non-empty. The default now keys off the number of real `is`
parents.

**A role's `token`/`rule` was global.** Regex declarations in a role body were
never classified as declarations, so they only ever reached a token table keyed
by whatever package happened to be current. Two roles declaring the same token
name aliased each other:

```raku
role A { token item { 'a' } }
role B { token item { 'b' } }
grammar GA does A { token TOP { <item> } }
grammar GB does B { token TOP { <item> } }
say ?GA.parse('a');   # was False — GA saw B's <item>
```

A `token`/`rule` in a role body is now registered under the *composing* type's
package, like the method it is.

Pinned by `t/role-body-composition-timing.t`, which Rakudo passes unchanged.
`t/run-nested-role-body.t` asserted the old declaration-time timing and was
corrected to the composition-time one.
