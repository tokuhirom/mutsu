use Test;

plan 6;

# A `for` block binds `$_` as its own implicit parameter, so it must shadow an
# enclosing `$_` — including one that came from a routine's `$_` PARAMETER,
# which occupies a local slot the loop used to leave untouched.

sub topic-values() {
    my @b;
    for 1, 2, 3 { @b.push($_) }
    @b;
}
is topic-values().join(','), '1,2,3', 'a plain for loop binds the topic';

sub param-topic($_) {
    my @b;
    for 1, 2, 3 { @b.push($_) }
    @b;
}
is param-topic(99).join(','), '1,2,3', 'a for loop shadows a `$_` parameter';

sub param-topic-restored($_) {
    my @b;
    @b.push($_);
    for 1, 2 { @b.push($_) }
    @b.push($_);
    @b;
}
is param-topic-restored(99).join(','), '99,1,2,99',
    'the `$_` parameter is restored after the loop';

# A nested for loop rebinds and restores the topic at each level.
sub nested-topic($_) {
    my @b;
    for 1, 2 -> $outer {
        for 'a', 'b' { @b.push("$outer$_") }
    }
    @b.push($_);
    @b;
}
is nested-topic(99).join(','), '1a,1b,2a,2b,99',
    'a nested for loop rebinds the topic at each level';

# The same through `given`/`when`, where the topic is re-bound twice.
class Shifter {
    method !bytes($_) {
        my @b;
        given $_ {
            when Int {
                my $num = 1;
                for 16, 8...0 { @b.push(($num +> $_) +& 0xFF) }
            }
        }
        @b;
    }
    method go() { self!bytes(5) }
}
is Shifter.go.join(','), '0,0,1',
    'a for loop inside a when block binds its own topic';

# A range-driven loop takes a different VM path; check it too.
sub param-topic-range($_) {
    my @b;
    for 1..3 { @b.push($_) }
    @b.push($_);
    @b;
}
is param-topic-range(99).join(','), '1,2,3,99',
    'a range for loop shadows and restores a `$_` parameter';
