use Test;

# The caller's argument-source names are per call site: a call that does not
# consume them (a light closure or method bind) must leave nothing behind for
# the next call, and a named argument naming a variable still aliases it.
plan 6;

class Point {
    has $.x;
    has $.y;
    method sum(Point $other) { $!x + $other.x }
}

my $p = Point.new(x => 1, y => 2);
my $total = 0;
for ^3 {
    my $q = Point.new(x => $_, y => $_ * 2);
    $total += $p.sum($q);
}
is $total, 6, 'calls in a loop bind their named arguments independently';

my $v = 10;
sub takes-raw(:$n is raw) { $n = 5 }
takes-raw(n => $v);
is $v, 5, 'a named argument naming a variable still aliases it for an `is raw` parameter';

my $w = 1;
my &closure = -> $a, :$b { $a + $b };
is closure($w, b => $w), 2, 'a light closure with a named argument';
is closure(3, b => 4), 7, 'and again with different arguments';

sub positional-then-named($a, $b?, :$c) { "$a|{$b // '-'}|{$c // '-'}" }
my $s = 'x';
is positional-then-named($s, c => $s), 'x|-|x', 'the previous call leaves no stale source names';
is positional-then-named($s, 'y'), 'x|y|-', 'and the next call binds positionally';
