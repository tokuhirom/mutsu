use v6;
use Test;

# The `CallMethodMut` plain-method lane (#8880) skips the pre-dispatch probe
# chain for a (class, method, argument type keys) triple once a dispatch has
# walked that chain unclaimed. Since #10111 it admits calls WITH arguments,
# keyed by each argument's type and definedness. Every call below runs in a
# loop long enough to install and then replay the lane, and must answer what
# a cold dispatch answers. Expectations checked against rakudo.

plan 9;

class Shape {
    multi method scale(Int:D $n) { $n * 2 }
    multi method scale(Rat $r) { ($r * 4).Int }
    multi method scale(Str $s) { $s.chars }
    multi method scale(Int:U $t) { 'type' }
    method pick-where($n where * > 10) { 'big' }
    method named($x, :$k = 'none') { "$x/$k" }
    method push($x) { "user-push:$x" }
}

my $s = Shape.new;
my @args = 3, 3/4, 'abcd', Int;
my @got;
for ^40 -> $i { @got.push: $s.scale(@args[$i % 4]) }
is @got[^8].join(','), '6,3,4,type,6,3,4,type',
    'multi candidates chosen by argument type, and definedness, on every pass';

my $big = 0;
for ^30 { $big++ if (try $s.pick-where(20)) eq 'big' }
is $big, 30, 'a where-constrained method keeps binding on the lane';
my $died = 0;
for ^30 { $died++ unless try $s.pick-where(5) }
is $died, 30, 'and keeps rejecting a value its where clause refuses, same type';

my @n;
for ^30 -> $i { @n.push: $i %% 2 ?? $s.named(1) !! $s.named(1, k => 'v') }
is @n[^4].join(','), '1/none,1/v,1/none,1/v', 'a named argument does not reuse the positional-only entry';

my @j;
for ^30 { @j.push: $s.scale(1|2) }
ok @j[29] ~~ Junction, 'a Junction argument still autothreads';
is @j[29].raku, any(2, 4).raku, 'to the candidate per eigenstate';

my @p;
for ^30 { @p.push: $s.push(7) }
is @p[29], 'user-push:7', 'a user method named like a native one is the one called';

Shape.^add_method('late', method ($x) { "late:$x" });
my @l;
for ^30 { @l.push: $s.late(1) }
is @l[29], 'late:1', 'a method added at run time is reachable';

class Sub is Shape { multi method scale(Int $n) { 'sub' } }
my $sub = Sub.new;
my @sb;
for ^30 { @sb.push: $sub.scale(5) }
is @sb[29], 'sub', 'a subclass keys apart from its parent';
