use Test;

# Source: LEB128 (zef distribution) -- a multi candidate whose omitted
# optional parameter is a native `int $o = 0` must still be selectable.
plan 5;

multi sub g(Int $v, int $o = 0) { "g$o" }
is g(1), 'g0', 'omitted native int optional with default';
# A literal is a boxed Int, so only a native variable satisfies `int` in a multi
# (rakudo 2026.09 refuses `g(1, 5)`).
my int $five = 5;
is g(1, $five), 'g5', 'supplied native int optional';

multi sub k(Buf $t, int $o = 0 --> int) { $o }
multi sub k(Int $v --> Buf) { Buf.new }
is k(Buf.new), 0, 'candidate with return type and omitted native int';

multi sub z(int $o = 0) { 'z' }
is z(), 'z', 'sole native int optional, no args';

multi sub n(Int $v, num $o = 0e0) { 'n' }
is n(1), 'n', 'native num optional';
