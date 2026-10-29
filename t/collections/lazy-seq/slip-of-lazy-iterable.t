use Test;

plan 8;

# `|EXPR` over a never-ending iterable stays lazy: it neither hangs nor caps
# the list, and `my @a = |EXPR` keeps the array lazy.

my @a = |(1..Inf);
is-deeply @a[^3], (1, 2, 3), 'my @a = |(1..Inf) is a lazy array';

my @b = |(1..Inf).map(* * 2);
is-deeply @b[^3], (2, 4, 6), 'my @b = |(1..Inf).map(...) is lazy';

my @c = |(lazy 1..Inf);
is-deeply @c[^3], (1, 2, 3), 'my @c = |(lazy 1..Inf) is lazy';

# A Range subclass with its own lazy iterator (Math::Sequences' Naturals).
class Nats is Range {
    method iterator {
        class :: does Iterator {
            has $.n = 0;
            method is-lazy() { True }
            method pull-one() { $!n++ }
        }.new
    }
}
my \N = Nats.new(0, Inf);
my @n = |N;
is-deeply @n[^4], (0, 1, 2, 3), '|$range-subclass with a lazy iterator does not hang';

my @m = (|N).map(* + 1)[^3];
is-deeply @m, [1, 2, 3], 'the slipped list is usable lazily';

# A Range subclass whose own iterator is finite is drained.
class Three is Range {
    method iterator {
        class :: does Iterator {
            has $.i = 0;
            method pull-one() { $!i < 3 ?? $!i++ !! IterationEnd }
        }.new
    }
}
my @t = |Three.new(0, 2);
is-deeply @t, [0, 1, 2], '|$range-subclass with a finite iterator slips its items';
is-deeply (7, |Three.new(0, 2), 8).List, (7, 0, 1, 2, 8), 'it slips inside a list too';

# A list that happens to start with a slipped finite range is unchanged.
my @f = |(1..3);
is-deeply @f, [1, 2, 3], 'a finite range still slips its elements';
