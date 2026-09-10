use Test;

# `fail` inside a TWEAK (or BUILD) submethod must make `.new` return a Failure
# — an unthrown exception that only throws when the object is used — not throw
# immediately at construction. A constructor that validates its arguments with
# `fail` relies on this (e.g. Math::Angle's TWEAK fails when zero or multiple
# angle units are given, and the caller `dies-ok { $obj }`).

plan 6;

class Angle {
    has $.rad;
    submethod TWEAK(:$rad, :$deg) {
        fail 'Specify exactly one of rad or deg'
            unless ($rad.defined xor $deg.defined);
        $!rad = $deg.defined ?? $deg / 57.29577951308232 !! $rad;
    }
}

my $bad = Angle.new;                 # neither -> fail
ok $bad ~~ Failure, 'new with no unit returns a Failure, not a throw';
dies-ok { $bad.rad }, 'using the Failure throws';

my $bad2 = Angle.new(:rad(1), :deg(90));   # both -> fail
ok $bad2 ~~ Failure, 'new with two units returns a Failure';

my $good = Angle.new(:rad(1));
ok $good !~~ Failure, 'valid construction is not a Failure';
is $good.rad, 1, 'valid construction stores its attribute';

# A `fail` in a plain method still returns a Failure (regression guard).
class M { method risky($bad) { fail 'no' if $bad; 42 } }
ok M.new.risky(True) ~~ Failure, 'fail in a plain method still returns a Failure';
