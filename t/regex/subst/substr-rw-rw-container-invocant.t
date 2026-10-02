use Test;

# `.substr-rw(...) = v` writes through the invocant's container, because
# Rakudo declares it with a raw invocant (`method substr-rw(\SELF: ...)`).
# That container may be an `is rw` attribute accessor's or an `is rw`
# parameter's, which no by-name writeback reaches. mutsu#10790.

plan 10;

class C { has $.s is rw = "abc" }
my $c = C.new;
$c.s.substr-rw(0, 1) = "Z";
is $c.s, 'Zbc', 'an `is rw` accessor invocant is written through';

sub f($x is rw) { $x.substr-rw(0, 1) = "M" }
my $m = "abc";
f($m);
is $m, 'Mbc', 'an `is rw` parameter invocant reaches the caller';

sub typed(Str $x is rw) { $x.substr-rw(0, 1) = "H" }
my $h = "abc";
typed($h);
is $h, 'Hbc', 'a typed `is rw` parameter too';

class D { has $.s is rw = "xyz"; method go { $!s.substr-rw(1, 1) = "_"; $!s } }
is D.new.go, 'x_z', 'a private attribute invocant';

my @es = C.new xx 2;
.s.substr-rw(0, 1) = "W" for @es;
is-deeply @es».s, ['Wbc', 'Wbc'], 'accessor invocant through the topic';

my $s = "hello";
my $r = ($s.substr-rw(1, 2) = "EE");
is $s, 'hEElo', 'a plain variable still works';
is $r, 'EE', 'the assignment is the stored substring';

my $t = "hello";
my $u := $t;
$u.substr-rw(0, 1) = "C";
is $t, 'Cello', 'a bound alias writes the shared container';

my %hash = a => "hello";
%hash<a>.substr-rw(0, 1) = "J";
is %hash<a>, 'Jello', 'a hash element still works';

my $len = 2;
my $v = "abcdef";
$v.substr-rw(1, $len) = "-";
is $v, 'a-def', 'a variable length is read through its container';
