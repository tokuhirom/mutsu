use lib 't/lib';
use Test;

# With an EXPORTHOW::SUPERSEDE class HOW in effect, a role mixed into the
# class's metaobject by a composed role's attribute trait is still there when
# the class finishes composing (a parameterized role is composed by the
# class declaration itself, ahead of the class's own registration step).

plan 4;

use SupersedeBuiltinHOW;


role Marker { }
role RoleMark {
    method specialize(Mu \r, Mu:U \obj, *@, *%) {
        obj.HOW does Marker unless obj.HOW ~~ Marker;
        nextsame;
    }
}
multi trait_mod:<is>(Attribute:D $a, :$marked!) {
    given $*PACKAGE.HOW {
        when Metamodel::ParametricRoleHOW { $_ does RoleMark unless $_ ~~ RoleMark }
        when Metamodel::ClassHOW { $_ does Marker unless $_ ~~ Marker }
    }
}

class Bar { }
role Param[::T] { has T $.a is marked; }
role Plain { has $.b is marked; }

class WithParam does Param[Bar] { }
class WithPlain does Plain { }

ok WithParam.HOW ~~ Marker, 'the metaobject of a class consuming a parameterized role has the mixin';
ok WithPlain.HOW ~~ Marker, 'and of a class consuming a plain role';
is WithParam.HOW.^name, 'CustomClassHOW+{Marker}', 'the superseding HOW is kept under the mixin';
isa-ok WithParam.new, WithParam, 'the class still constructs';
