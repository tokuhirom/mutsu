use v6;
use Test;

plan 8;

# `PHASER(...)` with no space is a call to a routine of that name; the phaser
# needs either a block or a space before the statement. Raku decides on exactly
# that: `BEGIN (1+2)` is the phaser, `BEGIN(1+2)` is a call.
# HTTP::Request::Common exports subs named POST/PUT that recurse as
# `POST($uri, ...)`, which mutsu used to swallow as a POST phaser.

sub POST($x) { "POST($x)" }
sub PRE($x) { "PRE($x)" }
sub LEAVE($x) { "LEAVE($x)" }
sub TEMP($x) { "TEMP($x)" }

sub call-them {
    my @r;
    @r.push: POST(1);
    @r.push: PRE(2);
    @r.push: LEAVE(3);
    @r.push: TEMP(4);
    @r
}
is-deeply call-them().List, ("POST(1)", "PRE(2)", "LEAVE(3)", "TEMP(4)"),
    'phaser-named subs are callable with parentheses';

# A routine whose whole body is such a call still returns its value.
sub tail-call { POST(9) }
is tail-call(), "POST(9)", 'a tail call to a phaser-named sub returns its value';

# The phasers themselves keep working.
sub with-post { POST { True }; 42 }
is with-post(), 42, 'the POST phaser block still parses';

is (BEGIN (1 + 2)), 3, 'BEGIN with a space takes a parenthesised statement';

my $left = 0;
sub with-leave { LEAVE { $left = 1 }; 7 }
is with-leave(), 7, 'the LEAVE phaser block still runs';
is $left, 1, 'and its body fired';

my @order;
sub with-enter { ENTER { @order.push: 'enter' }; @order.push: 'body'; 1 }
with-enter();
is-deeply @order.List, ('enter', 'body'), 'the ENTER phaser block still runs';

sub with-first {
    my @seen;
    for 1..3 { FIRST { @seen.push: 'first' }; @seen.push: $_ }
    @seen
}
is-deeply with-first().List, ('first', 1, 2, 3), 'the FIRST phaser block still runs';
