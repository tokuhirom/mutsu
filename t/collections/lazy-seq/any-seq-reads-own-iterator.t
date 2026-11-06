use v6;
use Test;

# `Any.Seq` is `Seq.new(self.iterator)`: a class that declares both `iterator`
# and a `list` written in terms of `.Seq` must not recurse through its own
# `list`. Found with BinaryHeap (`method list { self.Seq.list }`).
# Identical under raku.

plan 5;

class Walk does Iterator {
    has $.h; has $.p = 0;
    method pull-one() { $!p < 3 ?? $!h.get($!p++) !! IterationEnd }
}
role Box does Iterable {
    has @!a = 1, 2, 3;
    method get($i) { @!a[$i] }
    method iterator() { Walk.new(:h(self)) }
    method list() { self.Seq.list }
    method gist() { self.Seq.gist }
}
class Heap does Box {}

my $h = Heap.new;
is $h.list.join(","), "1,2,3", '.list through .Seq';
is $h.gist, "(1 2 3)", '.gist through .Seq';
is $h.Seq.join(","), "1,2,3", '.Seq reads the iterator';
is $h.map(* * 2).join(","), "2,4,6", '.map reads the iterator';
is $h.head(2).join(","), "1,2", '.head reads the iterator';
