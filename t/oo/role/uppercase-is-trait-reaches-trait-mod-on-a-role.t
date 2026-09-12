use Test;

# The role-side mirror of `uppercase-is-trait-reaches-trait-mod.t` (#8100).
#
# Raku spells a declarator-level trait `is Foo` and decides what it means from
# whether `Foo` names a known type, never from its capitalisation. The class
# path learnt that; the role path still deferred to `trait_mod:<is>` only for a
# LOWERCASE name, so `role Rr is Marked { }` -- the overwhelmingly common
# spelling of a trait -- died with `Unknown role: Marked`.
#
# The gate could not simply be dropped, because a role's `is Parent` and `does
# Parent` clauses are folded into the SAME synthetic statement: widening the
# deferral for both would have turned a `does` typo into an unknown-*parent*
# error. The declarator is now carried through to the runtime
# (`Stmt::DoesDecl::from_is` -> `RoleParentOp::from_is`), so only the `is` side
# widened and the `does` side is untouched -- which is what the last two
# assertions pin.

plan 10;

use MONKEY-SEE-NO-EVAL;

my %fired;
multi sub trait_mod:<is>(Mu:U $doee, :$Marked!)   { %fired{'Marked'} = $doee.^name }
multi sub trait_mod:<is>(Mu:U $doee, :$lowered!)  { %fired{'lowered'} = $doee.^name }
multi sub trait_mod:<is>(Mu:U $doee, :$Explodes!) { die "boom from handler" }

role Rr is Marked { method hi { 'hi' } }
role Gg is lowered { }

is %fired{'Marked'}, 'Rr', 'an uppercase `is Trait` on a role reaches the user trait_mod:<is>';
is Rr.^name, 'Rr', 'and the role is otherwise a normal role';
is Rr.^roles.elems, 0, 'the uppercase trait name did not become a composed role';

# The role still composes, and its methods arrive in the consuming class.
class C does Rr { }
is C.new.hi, 'hi', 'the role composes into a class and brings its methods';

is %fired{'lowered'}, 'Gg', 'a lowercase trait name on a role still reaches its handler';

# The trait argument is the named one, so a name no candidate claims is still
# an unknown parent rather than a silently-accepted trait.
throws-like 'role Beta is AlsoNotATrait { }', X::Inheritance::UnknownParent,
    'an uppercase name no candidate claims is still X::Inheritance::UnknownParent';

# An error raised from inside a matching handler still propagates as itself.
throws-like 'role Delta is Explodes { }', X::AdHoc,
    'an error from inside a matching uppercase handler still propagates';

# A known parent role reached through `is` is still ordinary composition.
{
    role Base { method who { 'base' } }
    role Derived is Base { }
    class D does Derived { }
    is D.new.who, 'base', 'a known uppercase `is` parent is still composed as a role';
}

# ...and so is one reached through `does`.
{
    role DoesBase { method who2 { 'does-base' } }
    role DoesDerived does DoesBase { }
    class E does DoesDerived { }
    is E.new.who2, 'does-base', 'a known `does` parent is unaffected';
}

# A `does` clause naming nothing is a TYPO, not a trait: it must keep dying
# even though a `trait_mod:<is>` is in scope. (rakudo reports `Invalid typename
# 'NoSuchRole'`; mutsu reports `Unknown role: NoSuchRole`. The point pinned
# here is that the widened `is` deferral did not swallow it.)
dies-ok { EVAL 'role Zeta does NoSuchRoleAtAll { }' },
    'an unknown `does` parent still dies';
