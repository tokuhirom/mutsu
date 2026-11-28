use Test;

# A resolved generated accessor is read at the front of the method-call site
# (`vm_accessor_lane`); everything below must answer as the full dispatch does,
# including after the site has been warmed by many reads.

plan 13;

class Animal {
    has $.name;
    has $.legs is rw;
    has @.tags;
    method describe() { $.name ~ " has " ~ $.legs ~ " legs" }
    method via-self() { self.name }
}
class Dog is Animal {
    has $.breed;
    method full() { self.describe() ~ " (" ~ $.breed ~ ")" }
}
class Robot is Animal {
    method name() { "robot-" ~ callsame() }
}

my @zoo = Animal.new(name => "Cat", legs => 4),
          Dog.new(name => "Rex", legs => 4, breed => "Lab"),
          Robot.new(name => "R2", legs => 0);

# Warm every site, then read once more (parent, child and overriding class
# alternate through the same `$.name` site in `describe`).
my @seen;
for ^50 { @seen = @zoo.map(*.describe) }
is-deeply @seen, ["Cat has 4 legs", "Rex has 4 legs", "robot-R2 has 0 legs"],
    'an inherited $.name site serves a parent, a child and an overriding class';

my $dog = @zoo[1];
my $s = '';
$s = $dog.full for ^50;
is $s, 'Rex has 4 legs (Lab)', 'nested self call reads both classes\' accessors';

my $r = '';
$r = $dog.via-self for ^50;
is $r, 'Rex', 'self.name in a method';

my $out = '';
$out = $dog.name for ^50;
is $out, 'Rex', '$obj.name on a variable';
is (@zoo[0].name for ^3).join(','), 'Cat,Cat,Cat', 'inline receiver';

# An `is rw` accessor still writes through after warm reads.
$dog.legs for ^50;
$dog.legs = 3;
is $dog.legs, 3, 'is rw accessor writes after warm reads';
is $dog.describe, 'Rex has 3 legs', 'and the warmed site reads the new value';

# An array attribute is decontainerized by the accessor.
my $a = Animal.new(name => 'A', legs => 1, tags => <x y>);
my $n;
$n = $a.tags.elems for ^50;
is $n, 2, '@.tags reads as an array';
is $a.tags.raku, '["x", "y"]', 'not itemized';

# A role mixin overriding the accessor wins over a warmed site.
my $cat = Animal.new(name => 'Tom', legs => 4);
$cat.name for ^50;
my $mixed = $cat but role { method name() { 'mixed' } };
is $mixed.name, 'mixed', 'a mixin method beats the warmed accessor';
is $mixed.describe, 'mixed has 4 legs', 'in an inherited method too';
is $cat.name, 'Tom', 'the original object is unaffected';

# An attribute holding a Proxy is fetched, not handed back.
class P {
    has $.p;
    method get() { $.p }
}
my $fetches = 0;
my $px = P.new(p => Proxy.new(FETCH => { $fetches++; 42 }, STORE => -> $, $ {}));
my $got;
$got = $px.get for ^5;
is $got, 42, 'a Proxy attribute is fetched through the accessor';
