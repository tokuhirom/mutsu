use Test;

# Subs reached through the frameless light call paths push no callframe
# entry; callframe(N) must still count them and report the line each one
# was executing.
plan 3;

sub ctx { callframe(2).line }
my sub foo { return ctx() }
my sub bar {
    my $y = 1;
    return foo();
}
my sub baz { return bar() }

is bar(), 12, 'callframe(2) is the intermediate sub frame';
is baz(), 12, 'callframe(2) skips frames of subs further out';

sub depth-line(Int $n) { callframe($n).line }
sub outer(Int $x) { return depth-line(3) }
sub wrap(Int $x) {
    my $z = $x;
    return outer($z);
}
is wrap(1), 25, 'callframe(3) through positional-light calls';
