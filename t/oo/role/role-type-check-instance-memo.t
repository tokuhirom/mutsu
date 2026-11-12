use Test;

# `$obj ~~ T` for an instance of a user class against a user class or role is
# remembered per (class, type) while the registry is unchanged (#17331). The
# answer must still follow everything that can change it: a composition made
# later, a lexical name that shadows the type, a subset, a smiley.

plan 15;

role R { }
role S { }
class C does R { }
class D { }
my $c = C.new;
my $d = D.new;

for ^3 {
    ok  $c ~~ R, 'an instance of a class doing R matches R';
    nok $c ~~ S, 'but not an unrelated role';
}
nok $d ~~ R, 'an instance of a class not doing R';
ok  $c ~~ C, 'an instance matches its own class';

# A runtime composition changes the object's class.
$d does S;
ok $d ~~ S, 'does S made D match S';

# A later declaration changes the registry, and the answers stay right.
role Late { }
class LC does Late { }
ok LC.new ~~ Late, 'a class declared later';
nok $c ~~ Late, 'and an older instance does not match it';

# A constant of the same name shadows the type in its scope.
sub shadowed { my constant R = 5; $c ~~ R }
nok shadowed(), 'a lexical constant named like the type is what ~~ sees';
ok $c ~~ R, 'and outside that scope the role is seen again';

# A subset decides on the value, not on the class.
class P { has $.n }
subset Big of P where *.n > 10;
ok  P.new(n => 20) ~~ Big, 'a subset accepts by value';
nok P.new(n => 1) ~~ Big, 'and rejects by value';
