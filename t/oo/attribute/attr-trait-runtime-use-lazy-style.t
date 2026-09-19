use lib 't/lib';
use Test;

# The reduction behind #8806: a distribution (Math::Matrix) that composes a
# role BEFORE `use`-ing a module whose custom `trait_mod:<is>` mixes a role
# into the Attribute meta-object AND, separately, mixes another role into the
# owning class's HOW (so its `compose` hook can install an accessor -- the
# real-world AttrX::Lazy shape). This used to die ONLY when the whole chain
# ran via a RUNTIME `use` (inside a sub) rather than a top-level one:
#
#   Can't use unknown trait 'is' -> 'lazy' in an attribute declaration.
#
# Three separate defects combined to cause it, all fixed independently
# (see mixin-attr-default-references-sibling.t and
# role-private-stub-not-required.t for two of them in isolation):
#  1. `RuntimeUseLazyStubRole`'s unimplemented PRIVATE stub method used to be
#     wrongly enforced as a composition requirement, which -- combined with
#     mutsu's forward-reference class hoisting -- made the whole class body
#     (including the attribute's trait dispatch) run TWICE.
#  2. The second run reused a WRONGLY cached Attribute meta-object: the
#     `trait_mod:<is>` handler's `$attr does AttrRole; ... $class.HOW does
#     HowRole` mixed into two different targets from the SAME handler call,
#     and a single writeback slot let the second (HOW) mixin overwrite the
#     first (attribute) one, so the cached value stopped type-checking as
#     `Attribute:D` on the second run.
# Fixing 1 and 2 makes runtime `use` behave exactly like a top-level one.

plan 2;

is EVAL(q[
    sub load { use RuntimeUseLazyTarget; "loaded" }
    load()
]), "loaded", 'runtime use of a module composing an incomplete-stub role, whose attribute trait mixes into both $attr and $class.HOW, does not die';

is EVAL(q[
    sub load {
        use RuntimeUseLazyTarget;
        my $a = RuntimeUseLazyTarget.^attributes.first(*.name eq '$!x');
        return $a.base-name;
    }
    load()
]), "x", 'the custom trait correctly mixed AttrRole into the attribute (survives the duplicate hoisted run)';
