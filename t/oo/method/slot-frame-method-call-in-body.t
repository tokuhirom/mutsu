use Test;

# A planned method whose body calls a method on a computed value
# (`($r * 4).Int`) runs in the slot frame, which has no env overlay of its
# own (ADR-18393, #19232). The callee then runs over the caller's env, so
# what it writes -- a dynamic variable, a captured outer variable -- must
# still reach the caller, an exception must still unwind to the caller's
# CATCH, and a `$CALLER::` lookup must still see the right frame. Each call
# repeats so the planned (second and later) calls are the ones checked.

plan 12;

my $*dyn = 0;
my $outer = 0;

class W {
    has $.v;
    method bump-dyn   { $*dyn++; $!v }
    method bump-outer { $outer++; $!v }
    method boom       { die "boom" }
    method caller-x   { $CALLER::x // 'none' }
}
multi infix:<+>(W $w, Int $i) { W.new(v => $w.v + $i) }

class C {
    method a($w) { ($w + 1).bump-dyn }
    method b($w) { ($w + 2).bump-outer }
    method d($w) { ($w + 0).boom }
    method e($r) { ($r * 4).Int }
    method g($w) { ($w + 5).caller-x }
}

my $c = C.new;
my $w = W.new(v => 7);

is (^3).map({ $c.a($w) }).join(','), '8,8,8', 'the callee answers';
is $*dyn, 3, "the callee's dynamic-variable writes reach the caller";
is (^3).map({ $c.b($w) }).join(','), '9,9,9', 'another callee answers';
is $outer, 3, "the callee's outer-variable writes reach the caller";

my @caught;
for ^2 { try { $c.d($w); CATCH { default { @caught.push: .message } } } }
is @caught.join(','), 'boom,boom', "the callee's exception reaches the caller's CATCH";
is $c.e(2), 8, 'the method still runs after an exception unwound through it';

is (^2).map({ $c.e(3/4) }).join(','), '3,3', 'a Rat argument';
is (^2).map({ $c.e(2) }).join(','), '8,8', 'an Int argument keys apart';

sub h { my $*x = 'dynx'; $c.g($w) }
is (^2).map({ h() }).join(','), 'none,none', '$CALLER:: from the callee sees the method frame, not its caller';

{
    my $*dyn = 100;
    is $c.a($w), 8, 'a callee under an inner dynamic';
    is $*dyn, 101, 'the write lands in the innermost dynamic';
}
is $*dyn, 3, 'the outer dynamic is untouched';
