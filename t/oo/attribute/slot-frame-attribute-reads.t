# A slot method frame (ADR-18393) reads scalar attributes from the receiver's
# attribute cell, and runs a body that divides (#19293). Every read must see
# the cell's current value: a write made by another method, by an rw
# accessor, by a user operator the body calls, or through a bound container.
# Each method is called many times so its call plan picks the slot frame.
use Test;

plan 10;

class Circle { has $.r; method area { 355/113 * $!r * $!r } }
class Rect { has $.w; has $.h; method area { $!w * $!h } }
class Tri { has $.b; has $.h; method area { $!b * $!h / 2 } }
my @shapes = Circle.new(r => 2), Rect.new(w => 3, h => 4), Tri.new(b => 5, h => 6);
my $t = 0;
$t += .area for |@shapes xx 100;
is $t, (355/113 * 4 + 12 + 15) * 100, 'polymorphic attribute arithmetic';

class Counter {
    has $.n is rw = 0;
    method twice { $!n * 2 }
    method bump { $!n = $!n + 1 }
}
my $c = Counter.new;
my @seen;
for ^50 { $c.bump; @seen.push($c.twice) }
is @seen[*-1], 100, 'a write by another method is seen';
$c.n = 7;
is $c.twice, 14, 'a write through the rw accessor is seen';

class Pub { has $.v; method dbl { $.v * 2 } }
my $p = Pub.new(v => 21);
my $s = 0;
$s += $p.dbl for ^100;
is $s, 4200, 'a public-twigil read';

class Wrap { has $.x; }
multi sub infix:<*>(Wrap $a, Int $b) { $a.x * $b }
class Holder { has $.w; has $.k; method prod { $!w * $!k } }
my $h = Holder.new(w => Wrap.new(x => 3), k => 5);
my $u = 0;
$u += $h.prod for ^100;
is $u, 1500, 'a user operator called on an attribute value';

class Mut {
    has $.a is rw;
    method sum { $!a * 1 + $!a * 1 }
}
my $m = Mut.new(a => 1);
my $mm = 0;
$mm += $m.sum for ^100;
is $mm, 200, 'two reads of one attribute';

class Bound { has $.x; method get { $!x * 1 }; method bind($c is rw) { $!x := $c } }
my $cell = 10;
my $b = Bound.new(x => 1);
my $g = 0;
$g += $b.get for ^100;
$b.bind($cell);
$cell = 42;
is $b.get, 42, 'a bound attribute container is read through';

role R { method area2 { 2 } }
class WithRole does R { has $.q; method qq { $!q * 3 } }
my $wr = WithRole.new(q => 4);
my $ww = 0;
$ww += $wr.qq for ^100;
is $ww, 1200, 'a class that does a role';

my $mixed = Rect.new(w => 2, h => 5) but role { method extra { 1 } };
my $mx = 0;
$mx += $mixed.area for ^100;
is $mx, 1000, 'a mixin receiver';

class Sub is Rect { }
my $sb = 0;
$sb += Sub.new(w => 6, h => 7).area for ^100;
is $sb, 4200, 'a subclass instance';
