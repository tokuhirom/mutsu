use Test;
use NativeCall;

plan 9;

class P is repr('CStruct') { has int32 $.x is rw }

my $a = CArray[P].new;
for ^8 { my $p = P.new; $p.x = $_; $a[$_] = $p }

# `Any.tail(n)` / `Any.reverse` are `self.list.tail(n)` / `self.list.reverse`:
# a CArray counts from the end of its own elements, not of itself.
is $a.tail.x, 7, 'tail without a count';
is $a.tail(2).elems, 2, 'tail(2) has two elements';
is $a.tail(2).map(*.x).join(','), '6,7', 'tail(2) is the last two elements';
is $a.tail(6).head.x, 2, 'the head of tail(6) is an element';
is $a.tail(100).elems, 8, 'a count beyond the end gives all of them';
is $a.reverse.elems, 8, 'reverse has every element';
is $a.reverse.head.x, 7, 'and starts from the last';
is $a.reverse.map(*.x).join(','), '7,6,5,4,3,2,1,0', 'reverse order';

my $s = CArray[int32].new(1, 2, 3, 4);
is $s.tail(2).join(','), '3,4', 'a native int CArray too';
