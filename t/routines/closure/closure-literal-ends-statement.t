use v6;
use Test;

plan 6;

# A statement whose text ends with a closure literal's `}` is self-terminating
# in Raku, exactly like one ending in `gather { ... }`. Without that, the next
# line's `if`/`for` is absorbed as a postfix modifier and its block runs
# unconditionally as a bare block. Rakudo's own Pod::To::Text opens with this
# shape (`my &colored = sub ($text, $) { $text }` then an `if %*ENV<...> {...}`).

my $ran = 0;
my &anon = sub ($text, $) { $text }
if 0 {
    $ran = 1;
}
is $ran, 0, 'block after a `sub {...}` initializer is a real if, not a bare block';
is anon('x', 1), 'x', 'the sub itself is still bound';

my $ran2 = 0;
my &pointy = -> $x { $x }
if 0 {
    $ran2 = 1;
}
is $ran2, 0, 'same for a pointy-block initializer';
is pointy(7), 7, 'the pointy block is still bound';

my @collected;
my &plain = sub { 1 }
for 1..3 -> $i {
    @collected.push($i);
}
is @collected.join(','), '1,2,3', 'a `for` on the next line loops as usual';

# On the SAME line a modifier still applies: the closure is only assigned when
# the condition holds.
my $assigned = 1;
$assigned = sub { 2 } if 0;
is $assigned, 1, 'same-line postfix modifier still governs the assignment';
