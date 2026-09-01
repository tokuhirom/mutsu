use v6;
use Test;

# Every evaluation of a gather expression creates a distinct block clone. Its
# `state` variables persist while that one gather is forced, but never collide
# with a sibling created from the same compiled body.
plan 4;

sub make() {
    gather {
        state $n = 0;
        take ++$n;
    }
}

is (make(), make()).map(*.head).join(','), '1,1',
    'strictly forced sibling gathers have separate state';

my @seen;
for ^3 {
    my @g = gather { state $n = 0; take ++$n; };
    @seen.push: @g[0];
}
is @seen.join(','), '1,1,1',
    'a gather literal evaluated in a loop gets a fresh state clone each time';

sub lazy-counter() {
    lazy gather { state $n = 0; loop { take ++$n } }
}

my $left = lazy-counter();
my $right = lazy-counter();
is $left[0], 1, 'first lazy gather starts its own state';
is "{$left[1]},{$right[0]},{$right[1]}", '2,1,2',
    'resuming either lazy gather retains only its own state';
