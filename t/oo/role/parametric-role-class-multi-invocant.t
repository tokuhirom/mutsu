use Test;

# Multi methods of a parameterized role, punned into a class, dispatch on a
# `::?CLASS:D:` invocant and on the argument's type, call after call; other
# classes' instances are not mistaken for the pun.

plan 8;

role Box[::T = Any, :&cmp = &infix:<cmp>] {
    has @!items;
    proto method add(|) {*}
    multi method add(::?CLASS:D: Pair:D $p) { @!items.push: "pair {$p.key}" }
    multi method add(::?CLASS:D: T:D $v)    { @!items.push: "value $v" }
    multi method add(::?CLASS:U: $)         { 'type object' }
    method all() { @!items.join('|') }
    method order($a, $b) { cmp($a, $b) }
}

my $b = Box.new;
$b.add($_) for 1, 2;
$b.add((a => 1));
$b.add(3);
is $b.all, 'value 1|value 2|pair a|value 3', 'candidates chosen per argument';
is Box.add(1), 'type object', 'the :U candidate for the type object';
is $b.order(1, 2), Less, 'the role parameter is the comparator';

my $r = Box[Int, :cmp(-> $a, $b { $b cmp $a })].new;
$r.add(5);
is $r.all, 'value 5', 'a parameterization dispatches its own candidates';
is $r.order(1, 2), More, 'with its own comparator';
throws-like { $r.add('x') }, X::Multi::NoMatch, 'and its own T';

class Other { method add($) { "other" } }
is Other.new.add(1), 'other', 'an unrelated class is not the pun';
ok $b ~~ Box, 'the instance still matches the role';
