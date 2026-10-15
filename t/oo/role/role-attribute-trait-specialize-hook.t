use v6.d;
use Test;

# An attribute trait on a role attribute mixes a role with a `specialize`
# method into the role's HOW (what AttrX::Mooish does). Composing the role
# into a class runs that hook BEFORE the role's attribute `compose`, so the
# hook can prepare the class HOW the attribute's `compose` relies on. A
# lexical (`my role`) and a package role both count, and the trait sees its
# own earlier `$*PACKAGE.HOW does ...` on the next `.HOW`.
plan 8;

my @log;
role Mark { method mark(Mu \t) { @log.push("mark {t.^name}") } }
role Hook {
    method specialize(Mu \r, Mu:U \obj, *@, *%) {
        @log.push("specialize {obj.^name}");
        obj.HOW does Mark unless obj.HOW ~~ Mark;
        nextsame
    }
}
my ($seen-in-trait, $seen-again);
multi trait_mod:<is>(Attribute:D $attr, :$mk!) {
    given $*PACKAGE.HOW {
        when Metamodel::ParametricRoleHOW {
            $_ does Hook unless $_ ~~ Hook;
            $seen-in-trait = $*PACKAGE.HOW ~~ Hook;
        }
        when Metamodel::ClassHOW { $_ does Mark unless $_ ~~ Mark }
    }
    $attr does role {
        method compose(Mu \type) {
            callsame;
            @log.push("compose {type.^name}");
            type.^mark;
        }
    }
}

role PkgRole { has $.a is mk; }
class PkgClass does PkgRole { }
{
    my role LexRole { has $.b is mk; }
    my class LexClass does LexRole { }
}
class Plain { has $.c is mk; }
role Quiet { has $.q; }
class QuietClass does Quiet { }
class Again does PkgRole { }

# Traits and composition run at compile time, so assert on the log by class.
sub events(Str $class) { @log.grep(*.ends-with(" $class")).map(*.words[0]).List }

ok $seen-in-trait, 'the trait sees the mixin it just applied to the role HOW';
is-deeply events('PkgClass'), <specialize compose mark>.List, 'package role: specialize runs before the attribute compose';
is-deeply events('LexClass'), <specialize compose mark>.List, 'lexical role: same order';
ok events('Plain').grep('specialize').elems == 0, 'a class attribute needs no specialize';
is QuietClass.new(q => 5).q, 5, 'a role whose HOW has no specialize hook composes unchanged';
is-deeply events('Again'), <specialize compose mark>.List, 'a second composing class runs the hook too';
ok PkgClass.HOW ~~ Mark, 'the first class HOW kept the mixin';
ok Again.HOW ~~ Mark, 'the second class HOW got the mixin';
