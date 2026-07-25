use v6;
use Test;

plan 5;

# A `CATCH` inside a BEGIN phaser handles that phaser's exceptions, including
# ones thrown out of a *call* inside it. Compiled inline into the mainline, the
# handler only covered a `die` at its own statement level, and anything
# unwinding from a call escaped as X::Comp::BeginTime.
# (HTTP::UserAgent's t/001-meta skips itself via exactly this shape.)

my @seen;

BEGIN {
    sub boom { die "from a call" }
    boom();
    CATCH { default { @seen.push: "begin-call:" ~ .message } }
}
is @seen[0], 'begin-call:from a call', 'CATCH in BEGIN catches a call that dies';

BEGIN {
    die "direct";
    CATCH { default { @seen.push: "begin-direct:" ~ .message } }
}
is @seen[1], 'begin-direct:direct', 'CATCH in BEGIN catches a direct die';

# A phaser with no handler still runs, and its declarations still reach the
# enclosing scope.
BEGIN { @seen.push: 'plain' }
is @seen[2], 'plain', 'a handler-less BEGIN is unchanged';

# `require` of a missing module is a catchable X::CompUnit::UnsatisfiedDependency
# — including in the `Test::` namespace, where a missing module is otherwise a
# deliberate no-op for `use`.
my $required-error;
BEGIN {
    require Test::DefinitelyNotInstalled;
    CATCH { default { $required-error = .^name } }
}
is $required-error, 'X::CompUnit::UnsatisfiedDependency',
    'a missing require inside BEGIN is catchable';

my $req2;
try {
    require Zork::AlsoNotInstalled;
    CATCH { default { $req2 = .^name } }
}
is $req2, 'X::CompUnit::UnsatisfiedDependency', 'and at runtime too';
