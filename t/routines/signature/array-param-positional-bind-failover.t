use Test;

# `binding_signature.rs` runs an implicit `PositionalBindFailover` check on
# every parameter bind (not just `@`-sigil ones -- the flag is computed
# before the sigil is even looked at). For an Int/Str/Range/... argument --
# a shape that can never compose the role -- that check used to walk the
# whole `type_matches` string gauntlet down to the final `dispatch_mro`
# fallback before answering `False`, on every single call. A RIPEMD-shaped
# hot loop (`-> blob32 $h, @words { ... }`, `-> [&f, $r, @K, $s] { ... }`)
# runs this on every round, so it was a measurable fraction of
# `t/ripemd.t`'s wall time (~38% of it, per the fast-path landed for this
# pin). The fast-reject added to `type_matches_value` short-circuits the
# provably-`False` cases directly; this pins that the answer stays correct,
# including for the handful of shapes the fast-reject deliberately leaves to
# the slow path (Seq/HyperSeq) and for the ones that must still fail to bind.
# A user class that explicitly composes `PositionalBindFailover` is also left
# to the slow path (it is a `ValueView::Instance`, not one of the plain
# shapes the reject matches on) -- untested here because binding such a
# class to an `@` parameter is independently broken in mutsu today
# (`.cache` is never invoked; see #8456), a pre-existing gap this change
# does not touch either way.

plan 8;

sub f(@x) { @x.join(',') }

is f([1, 2, 3]), '1,2,3', 'an Array argument still binds to an @ sigil parameter';
is f((1, 2, 3)), '1,2,3', 'a List argument still binds to an @ sigil parameter';
is f(1..3), '1,2,3', 'a Range argument still binds to an @ sigil parameter';
my $one = 1;
my $ab = 'ab';
my $hash = %(a => 1);
dies-ok { f($one) }, 'an Int argument still fails to bind to an @ sigil parameter';
dies-ok { f($ab) }, 'a Str argument still fails to bind to an @ sigil parameter';
dies-ok { f($hash) }, 'a Hash argument still fails to bind to an @ sigil parameter';

is f((1, 2, 3).Seq), '1,2,3', 'a Seq argument still binds to an @ sigil parameter';
is f(hyper map { $_ * 2 }, 1, 2, 3), '2,4,6',
    'a HyperSeq argument still binds to an @ sigil parameter';
