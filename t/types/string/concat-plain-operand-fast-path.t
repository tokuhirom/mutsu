# Infix `~` answers a plain Str / Int / Num operand without the coercion
# probes and without the Blob checks (#19293). Everything those probes and
# checks exist for must still behave as before: Blobs, holders around Blobs,
# Nil, type objects, zero-denominator Rationals, Proxies, user stringifiers.
# Each form runs in a loop so the hot (JIT) path is exercised too.
use Test;

plan 16;

my $n = 4;
my $s = 'legs';
my $f = 1.5e0;
my $out = '';
$out = "Rex has " ~ $n ~ " " ~ $s for ^300;
is $out, 'Rex has 4 legs', 'Str ~ Int ~ Str';
$out = $n ~ $n for ^300;
is $out, '44', 'Int ~ Int';
$out = $f ~ '|' ~ -0e0 ~ '|' ~ Inf for ^300;
is $out, '1.5|-0|Inf', 'Num operands render as .Str';
$out = 2**70 ~ '' for ^300;
is $out, '1180591620717411303424', 'a big Int';
$out = 1/3 ~ '' for ^300;
is $out, '0.333333', 'a Rat';

my $b = Buf.new(1, 2);
my $bb;
$bb = $b ~ Buf.new(3) for ^300;
is $bb.list, (1, 2, 3), 'Buf ~ Buf joins bytes';
is ("é".encode ~ "x"), 'éx', 'utf8 ~ Str decodes';
throws-like { Blob.new(1) ~ "x" }, X::Buf::AsStr, 'a non-utf8 Blob ~ Str dies';
is ($b.item ~ Buf.new(9)).list, (1, 2, 9), 'a Scalar around a Blob';

my $warned = 0;
{
    $out = Nil ~ 'x';
    CONTROL { when CX::Warn { $warned++; .resume } }
}
is $out, 'x', 'Nil stringifies to the empty string';
is $warned, 1, 'with a warning';
{
    $out = Int ~ 'y';
    CONTROL { when CX::Warn { $warned++; .resume } }
}
is $warned, 2, 'a type object warns too';
throws-like { 'a' ~ 1/0 }, X::Numeric::DivideByZero, 'a zero-denominator Rat dies';

my $p := Proxy.new(FETCH => -> $ { 5 }, STORE => -> $, $ { });
is 'x' ~ $p, 'x5', 'a Proxy FETCHes';

class C { method Str { 'cee' } }
$out = 'a' ~ C.new for ^300;
is $out, 'acee', 'a user Str method';
class D { method Stringy { 'dee' } }
is 'a' ~ D.new, 'adee', 'a user Stringy method';
