use Test;

# A built-in method takes its arguments by value: a `Proxy` argument is
# FETCHed before the method reads it. The element of a native `CArray` answers
# such a container (upstream's `AT-POS ... is raw` returns an `IntPosRef`),
# which is how `$blob.subbuf(0, $slen[0])` in OpenSSL's `RSAKey.sign` reads the
# length C wrote.
#
# Every expectation was verified against Rakudo.

plan 15;

my $p := Proxy.new(FETCH => method { 5 }, STORE => method ($v) { });
my $blob = Blob.new(1..10);
my @a = 1..10;

is $blob.subbuf(0, $p).bytes, 5, 'subbuf takes the FETCHed length';
is $blob.subbuf($p).bytes, 5, 'and the FETCHed start';
is "abcdefgh".substr(0, $p), 'abcde', 'substr takes the FETCHed length';
is @a.head($p).elems, 5, 'head takes the FETCHed count';
is (1..10).head($p).elems, 5, 'head on a Range';
is @a[^$p].elems, 5, 'prefix ^ takes the FETCHed bound';
is (1..$p).elems, 5, 'a range endpoint';
is "x" x $p, 'xxxxx', 'the repeat count';
is (1, 2, 3, 4, 5, 6)[$p], 6, 'a list subscript';
is -$p, -5, 'prefix -';

use NativeCall;
my $len = CArray[int32].new;
$len[0] = 4;
is $blob.subbuf(0, $len[0]).bytes, 4, 'a native CArray element as a method argument';
is @a[^$len[0]].elems, 4, 'and under ^';
is (1..$len[0]).elems, 4, 'and as a range endpoint';
is "abcdefg".substr(1, $len[0]), 'bcde', 'and for substr';

my $r := $len[0];
is $r.VAR.^name, 'IntPosRef', 'a bound element is still the reference';
