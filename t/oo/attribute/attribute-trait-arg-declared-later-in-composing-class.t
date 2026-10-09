use Test;

# Came from the Audio::Hydrogen ecosystem distribution: an attribute trait
# whose handler takes a typed named argument (`:&serialise!`), applied to a
# class that `does` a role, with the argument naming a `sub` declared in the
# class body. The role-composition stand-in pass sees the sub as Nil and must
# not turn that into an unknown-trait error.

plan 4;

my role Marker { }
my role Holder[&f] { has &.f = &f; }

multi sub trait_mod:<is> (Attribute $a, :&serialise!) {
    $a does Holder[&serialise];
}

class Foo does Marker {
    sub fv(Version $v --> Str) { $v.Str }
    has Version $.version is serialise(&fv) = Version.new("0.9.5");
}

is Foo.new.version.Str, '0.9.5', 'class with trait argument and role builds';
my $attr = Foo.^attributes.first(*.name eq '$!version');
ok $attr.f.defined, 'handler received the sub';
is $attr.f.(Version.new("1.2")), '1.2', 'the sub is the declared one';

# The same declaration evaluated at run time, where the argument is an
# undeclared routine for the stand-in pass.
lives-ok { EVAL q[
    class Bar does Marker {
        sub gv(Version $v --> Str) { $v.Str }
        has Version $.version is serialise(&gv);
    }
] }, 'EVAL-ed class with trait argument and role builds';
