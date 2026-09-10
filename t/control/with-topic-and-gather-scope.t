use Test;

plan 6;

# A block with an explicit signature does not bind `$_`: `with 1 -> $a { $_ }` leaves the
# outer topic alone, exactly as `for 1 -> $b { $_ }` already did.
$_ = 42;
with 1 -> $a {
    is $a, 1, 'the pointy parameter gets the value';
    is $_, 42, 'with -> $a leaves the outer topic alone';
}

# The parameterless form still topicalizes.
with 7 {
    is $_, 7, 'with without a signature topicalizes';
}

# This is what makes the rakudo#1695 case work: `.flip` inside the inner block reads the
# *outer* topic, not the `with` value.
$_ = 42;
is-deeply ((with 1 -> $a { .flip }) andthen $_), '24',
    'the inner block sees the enclosing topic';

# A `sub` declared in a gather block is lexical to it: two sibling gathers may each
# declare the same name, which used to be X::Redeclaration.
my $first = gather { sub emit-it($s) { take $s }; emit-it 'cc'; emit-it 'dd' };
my $second = gather { sub emit-it($n) { take $n * 2 }; emit-it 1; emit-it 2 };
is-deeply $first.List, ('cc', 'dd'), 'the first gather runs its own sub';
is-deeply $second.List, (2, 4), 'a sibling gather may redeclare the same sub';
