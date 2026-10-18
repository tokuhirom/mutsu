use Test;

plan 5;

my $iv = IntStr.new(42, "forty-two");
my int32 $n = $iv;
is $n, 42, 'an IntStr stores into a native int32';
my int $m = $iv;
is $m, 42, 'and into a native int';

sub widen(Int() $x) { my int32 $v = $x; $v }
is widen($iv), 42, 'Int() leaves an IntStr alone and the native store still works';
is widen($*USER), $*USER.Int, '$*USER (an IntStr) is accepted';
dies-ok { my int8 $b = IntStr.new(300, "x") ; $b == 300 or die }, 'range handling is unchanged for an allomorph';
