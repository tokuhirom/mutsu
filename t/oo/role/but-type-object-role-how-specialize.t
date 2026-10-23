use Test;

plan 7;

# `Type but R` specializes R onto the mixin type, so a `specialize` that an
# attribute trait mixed into R's HOW sees the type object, and may refuse it.
my @seen;
role Hook {
    method specialize(Mu \r, Mu:U \obj, *@, *%) {
        @seen.push: obj.^name;
        die "refused: " ~ obj.^name unless obj.HOW ~~ Metamodel::ClassHOW;
        nextsame;
    }
}
multi trait_mod:<is>(Attribute:D $attr, :$hooked!) {
    $*PACKAGE.HOW does Hook unless $*PACKAGE.HOW ~~ Hook;
}

role R { has $.a is hooked; }
role R2 { has $.b is hooked; }
class Foo { }

# The metaobject classes are NQP classes in Rakudo: their own metaobject is
# not a Raku ClassHOW.
nok Metamodel::ClassHOW.HOW ~~ Metamodel::ClassHOW, 'Metamodel::ClassHOW.HOW is not a ClassHOW';
is Metamodel::ClassHOW.HOW.^name, 'NQPClassHOW', 'and is an NQPClassHOW';
ok Foo.HOW ~~ Metamodel::ClassHOW, 'a user class still has a ClassHOW';

# Before anything composed R2, so the attribute trait has not run yet.
throws-like { Metamodel::ClassHOW but R2 }, X::AdHoc, message => /refused/,
    'specialize can refuse a native type object';

lives-ok { Foo but R }, 'a Raku class type object takes the role';
ok @seen.grep('Foo+{R}'), 'specialize saw the mixin type object';
throws-like { Metamodel::ClassHOW but R }, X::AdHoc, message => /refused/,
    'and refuses a native type object once the trait ran';
