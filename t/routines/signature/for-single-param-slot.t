use Test;

# A single `for` parameter binds each element; closures capture each
# iteration's binding, nested loops reusing the name see their own, and after
# the loop the name is the enclosing binding again.

plan 10;

my @subs;
for 1..3 -> $x { @subs.push: -> { $x * 10 } }
is @subs.map({ .() }).join(','), '10,20,30', 'closures capture each iteration';

my @r;
for 1..2 -> $x { for <a b> -> $x { @r.push: $x }; @r.push: $x }
is @r.join, 'ab1ab2', 'a nested loop reusing the name';

my $y = 'outer';
sub f() { for 1..2 -> $y { }; $y }
is f(), 'outer', 'after the loop the captured outer lexical is back';

my $z = 'z';
for 1..2 -> $z { }
is $z, 'z', 'after the loop the outer lexical is back';

my @n;
for 1..5 -> $i { next if $i == 2; last if $i == 4; @n.push: $i }
is @n.join, '13', 'next and last';

my @a = 1, 2, 3;
for @a -> $e is rw { $e *= 2 }
is @a.join(','), '2,4,6', 'is rw writes back';

for @a -> $e is copy { $e = 0 }
is @a.join(','), '2,4,6', 'is copy does not';

throws-like { for 1 -> $e { $e = 5 } }, Exception, message => /readonly/, 'a plain parameter is readonly';

class C { has $.v; method sum(@l) { my $s = 0; for @l -> $o { $s += $o.v }; $s } }
is C.new.sum((C.new(:v(1)), C.new(:v(2)))), 3, 'a loop in a method';

my @g = gather for 1..3 -> $k { take $k * $k };
is @g.join(','), '1,4,9', 'a gather over a loop';
