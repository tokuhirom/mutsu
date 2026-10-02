use Test;

# #10961: the method/sub call fast paths take their method name, owner
# class, parameter names, source file and the callee's `&name` from symbols
# interned once rather than re-interning the text on every call. These pin
# the behaviour each of those names drives, over repeated calls so the
# cached (second and later) dispatch is the one exercised.

plan 12;

class Counter {
    has $.n = 0;
    method bump($by) { $!n += $by; self }
    method name-of() { &?ROUTINE.name }
    method package-of() { $?CLASS.^name }
    method try-write($x) { $x = 5; $x }
    method write-rw($x is rw) { $x = 7 }
    method write-copy($x is copy) { $x = 9; $x }
}

my $c = Counter.new;
$c.bump($_) for 1..10;
is $c.n, 55, 'a one-parameter method binds its argument on every call';

is (^5).map({ $c.name-of }).join(','), 'name-of,name-of,name-of,name-of,name-of',
    '&?ROUTINE.name is the method name on repeated calls';
is (^3).map({ $c.package-of }).join(','), 'Counter,Counter,Counter',
    '$?CLASS is the owner class on repeated calls';

for ^3 {
    dies-ok { $c.try-write(1) },
        'a plain method parameter stays read-only on a repeated call';
}

my $v = 0;
$c.write-rw($v) for ^3;
is $v, 7, 'an is rw method parameter writes through on repeated calls';
is (^3).map({ $c.write-copy(1) }).join(','), '9,9,9',
    'an is copy method parameter is writable on repeated calls';

# A read-only parameter must not leak its mark into a caller variable that
# shares its name.
my $x = 1;
try $c.try-write(2) for ^2;
$x = 3;
is $x, 3, 'a caller variable sharing a parameter name stays writable';

class Selfish {
    method who(Selfish $self: ) { $self.^name }
}
is (^3).map({ Selfish.new.who }).join(','), 'Selfish,Selfish,Selfish',
    'a named invocant parameter binds on repeated calls';

# The callee's `&name`: a lexical `&f` parameter shadows the package `f`
# on every call.
sub f() { 'package' }
sub call-it(&f) { f() }
is (^3).map({ call-it(sub { 'lexical' }) }).join(','), 'lexical,lexical,lexical',
    'a &name parameter shadows the package routine on repeated calls';
is (^3).map({ f() }).join(','), 'package,package,package',
    'the package routine answers once no lexical &name shadows it';
