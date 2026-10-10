use Test;

# `::CALLERS::<...>` (leading `::`) is the same pseudo-stash term as
# `CALLERS::<...>`, and a `&` key finds a routine of a caller's scope.
# Found via Red's `::CALLERS::<&__RED_OPERATOR_LOADED__>`, distribution
# RedX::HashedPassword.

plan 5;

sub __loaded { True }
my $*d = 7;
sub callee {
    is CALLERS::<$*d>, 7, 'CALLERS::<$*d>';
    is ::CALLERS::<$*d>, 7, '::CALLERS::<$*d>';
    ok so(CALLERS::<&__loaded>), 'CALLERS::<&routine> finds a caller-scope routine';
    ok so(::CALLERS::<&__loaded>), '::CALLERS::<&routine>';
    ok !so(::CALLERS::<&__no_such_routine__>), 'unknown routine is falsy';
}
sub mid { callee() }
mid();
