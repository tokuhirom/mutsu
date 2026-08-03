use v6;
use Test;

# rakudo's `Instant` and `Duration` are `Cool` types that `does Real`
# (`Instant.^mro` is `((Instant) (Cool) (Any) (Mu))`), so `now ~~ Numeric` is
# True and `Real.abs` applies to both -- keeping the type, because it is
# `self < 0 ?? -self !! self` on the value itself.
#
# mutsu had `Duration` under `Real` only, and neither under `Numeric` or `Cool`.
# That is not academic: `is-approx $a, $b, :abs-tol(5)` over two Instants matched
# NONE of the real `Test.rakumod`'s `is-approx(Numeric, Numeric, ...)`
# candidates and fell through to mutsu's native provider, which keeps its own
# counter -- so the test count reset mid-file and the plan check failed on a file
# that had emitted every assertion (roast/S28-named-variables/init-instant.t).

plan 12;

my $i = now;
my $d = now - now;

ok $i ~~ Numeric, 'an Instant is Numeric';
ok $i ~~ Real,    'an Instant is Real';
ok $i ~~ Cool,    'an Instant is Cool';
ok $d ~~ Numeric, 'a Duration is Numeric';
ok $d ~~ Real,    'a Duration is Real';
ok $d ~~ Cool,    'a Duration is Cool';

isa-ok $i.abs, Instant,  '.abs on an Instant keeps the type';
isa-ok $d.abs, Duration, '.abs on a Duration keeps the type';

my $neg = Duration.new(-42);
is $neg.abs.Int, 42, '.abs on a negative Duration is its magnitude';
isa-ok $neg.abs, Duration, '...and still a Duration';

# A Numeric-constrained routine accepts them -- the shape that made the real
# Test module fall through to the native provider.
sub takes-numeric(Numeric $x, Numeric $y) { 'matched' }
is takes-numeric($i, $i), 'matched', 'a Numeric-constrained sub accepts Instants';
is takes-numeric($d, $d), 'matched', '...and Durations';
