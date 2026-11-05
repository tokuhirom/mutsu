use Test;
use NativeCall;

plan 8;

# The imported `postcircumfix:<[ ]>` family of NativeCall is a captured multi
# dispatcher; its verdict for a given argument-type key is reused on repeat calls.
my $c = CArray[num64].new;
$c[$_] = $_ * 1.5e0 for ^6;

my @seen;
@seen.push($c[$_]) for 0..5;
is-deeply @seen, [0e0, 1.5e0, 3e0, 4.5e0, 6e0, 7.5e0], 'repeated Int index reads agree';

my $i = 2;
is $c[$i], 3e0, 'a variable index';
is $c[2], 3e0, 'a literal index';
is $c[1, 3].join(','), '1.5,4.5', 'a list index takes the other candidate';
is $c[1, 3].join(','), '1.5,4.5', 'and again after the first settle';
is $c[4], 6e0, 'an Int read after a slice read still takes the scalar candidate';

multi sub tie(Int $x, Any $y) { 'int-any' }
multi sub tie(Any $x, Int $y) { 'any-int' }
my @r;
for ^3 { @r.push: (try tie(1, 2)) // 'ambiguous' }
is-deeply @r, ['ambiguous' xx 3].List.Array, 'an ambiguous dispatch stays ambiguous on every call';

multi sub dflt(Int $x) is default { 'default' }
multi sub dflt(Int $x) { 'other' }
is-deeply (^3).map({ dflt(1) }).List, ('default' xx 3).List, 'is default keeps winning';
