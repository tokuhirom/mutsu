use Test;
use MONKEY-TYPING;

# The plain `CallMethod` opcode answers a pure native method on an immutable
# scalar receiver before its scoped-env flatten guard (#7563), the way
# `CallMethodMut` already did (#7554). These pin the cases the gate must NOT
# steal from the general dispatch path, and the lexical views that must still
# be whole after a gated call.

plan 16;

# --- the gated shape itself -------------------------------------------------
class P {
    has $.x;
    has $.y;
    method dist() { (($!x * $!x) + ($!y * $!y)).sqrt }
}
is P.new(x => 3, y => 4).dist, 5e0, 'native method on a scalar receiver inside a method body';

# A gated call must not starve a later closure capture in the same frame: the
# capture needs the whole lexical view, which the skipped flatten used to
# materialize eagerly.
sub capture-after-native() {
    my $outer = 41;
    my $n = (4e0).sqrt;          # gated: native on a Num
    my $c = sub { $outer + $n };
    $outer = 40;
    $c();
}
is capture-after-native(), 42e0, 'a closure capture after a gated call sees the whole lexical view';

# ... and neither may a pseudo-stash read.
sub stash-after-native() {
    my $here = 7;
    my $ignored = (9e0).sqrt;    # gated
    MY::<$here>;
}
is stash-after-native(), 7, 'MY:: after a gated call still resolves an outer-declared lexical';

# --- cases the gate must leave to the general path --------------------------
augment class Str { method shout() { self.uc ~ '!' } }
sub augmented() { 'hi'.shout }
is augmented(), 'HI!', 'an augmented Str method still reaches the general path';

sub junction-arg() { 'abc'.contains(any('a', 'z')) }
ok junction-arg() ~~ Junction, 'a junction argument still autothreads instead of being gated';

sub quoted-name() { 'ab'."uc"() }
is quoted-name(), 'AB', 'a quoted method name still reaches the general path';

sub modifier-maybe() { 'ab'.?uc }
is modifier-maybe(), 'AB', '.? modifier still reaches the general path';

sub dot-return() { 'early'.return; 'late' }
is dot-return(), 'early', '.return on a Str still returns from the enclosing sub';

sub var-on-scalar() { my $v = 5; $v.VAR.WHAT }
ok var-on-scalar() ~~ Scalar, '.VAR on a scalar still reaches the general path';

# --- values, types, and failure modes are unchanged -------------------------
is (2e0).sqrt, 2e0.sqrt, 'Num.sqrt';
is 255.base(16), 'FF', 'Int.base with an argument';
is 'Hello'.substr(1, 3), 'ell', 'Str.substr with arguments';
is True.Int, 1, 'Bool.Int';
is 'abc'.chars, 3, 'Str.chars';
is (-7).abs, 7, 'Int.abs';
dies-ok { 'abc'.no-such-method-here }, 'an unknown method on a Str still dies';
