use Test;

# Raku spells a class-level trait `is Foo`, and decides what that means from
# whether `Foo` names a known type: if it does, this is inheritance; if it does
# not, it desugars to the NAMED argument `trait_mod:<is>($type, :Foo)`. mutsu
# only ever deferred an unknown parent to that dispatch when the name began
# with a LOWERCASE letter, so the overwhelmingly common spelling — a
# capitalised trait name — became X::Inheritance::UnknownParent instead of
# reaching the user's handler. That is what made the `Staticish`
# distribution's documented usage,
#
#     use Staticish;
#     class Foo is Static { ... }        # multi trait_mod:<is>(Mu:U, :$Static!)
#
# die on the class declaration.
#
# A deferred name is a TRAIT, not a parent, so it must also stay out of the
# class's C3 parents: rakudo reports `Alpha.^parents` as empty and
# `Alpha.^mro` as `Alpha, Any, Mu`. mutsu left the name in the parent list on
# BOTH paths, giving every such class a phantom ancestor.

plan 9;

use MONKEY-SEE-NO-EVAL;

my %fired;
multi sub trait_mod:<is>(Mu:U $doee, :$Marked!) { %fired{'Marked'} = $doee.^name }
multi sub trait_mod:<is>(Mu:U $doee, :$lowered!) { %fired{'lowered'} = $doee.^name }
multi sub trait_mod:<is>(Mu:U $doee, :$Explodes!) { die "boom from handler" }

class Alpha is Marked { method hi { 'hi' } }
class Gamma is lowered { }

is %fired{'Marked'}, 'Alpha', 'an uppercase `is Trait` reaches the user trait_mod:<is>';
is Alpha.new.hi, 'hi', 'and the class is otherwise a normal class';
is Alpha.^parents.elems, 0, 'the uppercase trait name did not become a parent';
is Alpha.^mro.map(*.^name).join(','), 'Alpha,Any,Mu', 'nor an MRO entry';

is %fired{'lowered'}, 'Gamma', 'a lowercase trait name still reaches its handler';
is Gamma.^mro.map(*.^name).join(','), 'Gamma,Any,Mu',
    'and it is likewise absent from the MRO';

# The trait argument is the named one, so a name no candidate claims is still
# an unknown parent rather than a silently-accepted trait.
throws-like 'class Beta is AlsoNotATrait { }', X::Inheritance::UnknownParent,
    'an uppercase name no candidate claims is still X::Inheritance::UnknownParent';

# An error raised from inside a matching handler still propagates as itself.
throws-like 'class Delta is Explodes { }', X::AdHoc,
    'an error from inside a matching uppercase handler still propagates';

# A real parent that happens to sit alongside the traits is still inheritance.
{
    class Base { method who { 'base' } }
    class Derived is Base { }
    is Derived.new.who, 'base', 'a known uppercase parent is still ordinary inheritance';
}
