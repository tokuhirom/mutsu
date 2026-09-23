# No `use Test` here on purpose: loading a module at the top of the file
# changes how the frame's locals are mirrored into env and hides the bug, so
# this file writes its TAP by hand.

say '1..8';
my $n = 0;
sub is($got, $expected, $desc) {
    $n++;
    my $ok = $got eqv $expected;
    say ($ok ?? 'ok' !! 'not ok'), " $n - $desc";
    say "# expected: {$expected.raku}\n#      got: {$got.raku}" unless $ok;
}

# `eager gather { ... }` inside a loop used to reset outer locals that the
# loop body wrote to in its own frame slot (issue #9165). Forcing the gather
# copied every env entry back over the frame's slots, and a slot-only local's
# env entry is stale, so `$t` went back to 0 before each increment. Only a
# name the gather body actually wrote may be copied back.

sub counter-after-eager() {
    my $t = 0;
    for ^3 { my @a = eager gather { take 1 }; $t += 1 }
    $t
}
is counter-after-eager(), 3, 'a slot write after eager gather survives the next iteration';

sub counter-bare-eager() {
    my $t = 0;
    for ^5 { eager gather { take 1 }; $t += 1 }
    $t
}
is counter-bare-eager(), 5, 'the same with a bare (sunk) eager gather';

{
    my $t = 0;
    for ^3 { my @a = eager gather { take 1 }; $t += 1 }
    is $t, 3, 'the same inside a bare block';
}

# The gather body's own writes to an outer variable still propagate.
sub counter-in-gather() {
    my $u = 0;
    for ^3 { my @a = eager gather { $u += 1; take 1 } }
    $u
}
is counter-in-gather(), 3, 'a write inside the gather body reaches the outer local';

sub both-sides() {
    my $u = 0;
    my $t = 0;
    for ^3 { $t++; my @a = eager gather { $u += 1; take 1 }; $t++ }
    "$u $t"
}
is both-sides(), '3 6', 'writes on both sides of the eager gather all count';

my $was-lazy = 1;
my @a = eager gather { $was-lazy = 0; take 1 };
is $was-lazy, 0, 'a top-level write inside the gather body propagates';

sub push-log() {
    my @log;
    for ^3 { my @b = eager gather { @log.push($_); take 1 }; @log.push('x') }
    @log.join(' ')
}
is push-log(), '0 x 1 x 2 x', 'array pushes inside and after the gather interleave';

sub take-counter() {
    my @seen;
    my $t = 0;
    for ^3 { $t = $t + 1; my @c = eager gather { take $t }; @seen.push(@c[0]) }
    @seen.join(' ')
}
is take-counter(), '1 2 3', 'the gather body reads the current value of the outer local';
