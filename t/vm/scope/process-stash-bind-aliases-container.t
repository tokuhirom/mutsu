use Test;

# `PROCESS::<$x> := SOURCE` makes the dynamic `$*x` the source's own
# container: a later write to either side is seen through the other.
# IO::CatHandle::AutoLines (used by the LN distribution) publishes its line
# counter this way: `PROCESS::<$LN> := $!ln` in a TWEAK.

plan 11;

{
    my $x = 1;
    PROCESS::<$PSB-A> := $x;
    $x = 11;
    is $*PSB-A, 11, 'a write to the variable is seen through the dynamic';
    $*PSB-A = 9;
    is $x, 9, 'a write to the dynamic reaches the variable';
    $x = 12;
    is $*PSB-A, 12, 'the alias survives a write through the dynamic';
}

class Counter {
    has Int $.n is rw = 0;
    submethod TWEAK { PROCESS::<$PSB-COUNT> := $!n }
    method bump { ++$!n }
}

{
    my $c = Counter.new;
    is $*PSB-COUNT, 0, 'an attribute bound in TWEAK starts at its value';
    $c.bump;
    $c.bump;
    is $*PSB-COUNT, 2, 'a method incrementing the attribute is seen through the dynamic';
    $c.n = 10;
    is $*PSB-COUNT, 10, 'an accessor assignment is seen through the dynamic';
}

{
    my $y = 5;
    PROCESS::<$PSB-T> := $y;
    $y = 6;
    sub peek { $*PSB-T }
    is peek(), 6, 'a routine reads the current value through the dynamic';
    is (await start { $*PSB-T }), 6, 'another thread reads it too';
}

{
    class Box { has $.p }
    my $first = Box.new(:p<first>);
    my $second = Box.new(:p<second>);
    PROCESS::<$PSB-RB> := $first;
    PROCESS::<$PSB-RB> := $second;
    is $*PSB-RB.p, 'second', 'a second bind replaces the first';
    PROCESS::<$PSB-RB> := $first;
    is $*PSB-RB.p, 'first', 'binding back restores the first';
}

{
    PROCESS::<$PSB-RO> := 42;
    throws-like { $*PSB-RO = 1 }, X::Assignment::RO,
        'binding a value (not a container) stays read-only';
}
