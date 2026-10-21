use Test;

# `|$obj` of an `is Hash` / `is Array` subclass instance slips its contents,
# as `|%h` / `|@a` do: pairs become named arguments. PDF's
# `self!encode-item($input, |$dict)` passes a `PDF::COS::Dict` (an `is Hash`
# class) this way. Expected values are the `raku` (v2026.09) output.

plan 6;

class D is Hash {}
my $d = D.new;
$d<a> = 1;
sub f(:$a) { $a }
is f(|$d), 1, 'a Hash subclass slips its pairs as named arguments';

class P {
    method !g(:$a!) { $a }
    method h($x) { self!g(|$x) }
}
is P.h($d), 1, '... into a private method too';
is P.h({ a => 2 }), 2, 'a plain Hash still does';
is (|$d).raku, 'slip(:a(1),)', 'the slip holds the pairs';

class A is Array {}
my $arr = A.new(1, 2);
sub g($x, $y) { $x + $y }
is g(|$arr), 3, 'an Array subclass slips its elements as positionals';
is (0, |$arr, 3).join(','), '0,1,2,3', '... and into a list';
