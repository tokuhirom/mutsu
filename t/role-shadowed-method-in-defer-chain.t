use v6;
use Test;

plan 6;

# ADR-0019 E9-pre finding (todo/tickets/role-shadowed-method-in-defer-chain.md):
# a `does`-composed role method that the class overrides by the same name must
# NOT be a `nextsame`/`callsame` chain entry -- it is fully shadowed, exactly
# as if the role method never existed. Ground truth verified against Rakudo
# v2026.06. (A runtime mixin via `but`/`does` on an instance is a DIFFERENT
# shape -- it keeps its own MRO entry in both raku and mutsu -- and is already
# covered by t/nextsame-role-mixin.t.)

{
    role RoleShadow1 { method m() { "r" } }
    class ClassShadow1 does RoleShadow1 { method m() { callsame } }
    is ClassShadow1.new.m, Nil, 'callsame from a class method that shadows a role method of the same name sees nothing to defer to';
}

{
    role RoleShadow2 { method m() { "r" } }
    class ClassShadow2 does RoleShadow2 {
        method m() {
            my $r = callsame;
            "c-{$r // 'Nil'}";
        }
    }
    is ClassShadow2.new.m, 'c-Nil', 'the shadowed role method never runs -- callsame yields Nil, not the role body';
}

# Same-signature multi pair: the role's candidate is a dropped flattened
# duplicate in raku, so callsame from the class's own candidate also yields
# Nil.
{
    role RoleShadow3 { multi method m(Int $x) { "r-int" } }
    class ClassShadow3 does RoleShadow3 {
        multi method m(Int $x) {
            my $r = callsame;
            "c-{$r // 'Nil'}";
        }
    }
    is ClassShadow3.new.m(1), 'c-Nil', 'same-signature multi pair: role candidate is a dropped duplicate, callsame yields Nil';
}

# A role candidate with a DIFFERENT signature keeps participating normally --
# this ticket only excludes same-name-and-signature shadowing.
{
    role RoleShadow4 {
        multi method m(Int $x) { "r-int" }
        multi method m(Str $x) { "r-str" }
    }
    class ClassShadow4 does RoleShadow4 {
        multi method m(Int $x) {
            my $r = callsame;
            "c-{$r // 'Nil'}";
        }
    }
    is ClassShadow4.new.m(1), 'c-Nil', 'same-signature Int candidate is still shadowed';
    is ClassShadow4.new.m('x'), 'r-str', 'different-signature Str candidate is not shadowed and still dispatches directly';
}

# A role-qualified call still reaches the role's own method (a different path
# than the unqualified nextsame/callsame chain -- unaffected by this ticket).
{
    role RoleShadow5 { method m() { "r" } }
    class ClassShadow5 does RoleShadow5 { method m() { self.RoleShadow5::m() } }
    is ClassShadow5.new.m, 'r', 'a role-qualified call still reaches the role method directly';
}

done-testing;
