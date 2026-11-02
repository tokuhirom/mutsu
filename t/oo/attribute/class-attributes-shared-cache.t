use Test;
plan 9;
class A { has Int $.x = 1; has @.xs; has %.h; method bump { $!x++; self } }
my $a = A.new;
is $a.bump.bump.x, 3, 'accessor/mutator through the cached attribute list';
is A.new.x, 1, 'a fresh instance seeds the default';
is $a.xs.elems, 0, 'array attribute seeded empty';
class B is A { has $.y = 5; has Str $.x2 = "q" }
is B.new.y, 5, 'subclass attribute default';
is B.new.x, 1, 'inherited attribute default';
is B.new(x => 7).x, 7, 'named init of an inherited attribute';
is A.new.^attributes.elems, 3, 'A still reports its own three attributes after B exists';
class C { has $.v is rw = 2 }
my $c = C.new;
$c.v = 9;
is $c.v, 9, 'rw accessor assignment';
is C.new.v, 2, 'default unaffected';
