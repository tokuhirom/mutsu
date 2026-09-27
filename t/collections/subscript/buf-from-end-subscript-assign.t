use Test;

# A from-end WhateverCode subscript (`*-1`) on a Buf resolves against the
# buffer's element count on assignment, as it already did on read. It used to
# see length 0 and die with "Index out of range" (the EC dist's ed25519 clamps
# a scalar with `$s[*-1] +&= 0b0111_1111`).

plan 6;

my $s = buf8.new(1, 2, 3);
$s[*-1] = 9;
is $s[2], 9, 'plain assignment to $buf[*-1]';
$s[*-1] += 1;
is $s[2], 10, 'compound assignment to $buf[*-1]';
$s[*-1] +&= 0b0000_0110;
is $s[2], 2, '+&= on $buf[*-1]';
$s[*-1] +|= 0b0100_0000;
is $s[2], 66, '+|= on $buf[*-1]';
$s[*-3, *-2] = 7, 8;
is-deeply $s.list, (7, 8, 66), 'from-end slice assignment';

sub clamp {
    my buf8 $k .= new: blob8.new(1 .. 64).subbuf(0, 32);
    $k[0]   +&= 0b1111_1000;
    $k[*-1] +&= 0b0111_1111;
    $k[*-1] +|= 0b0100_0000;
    ($k[0], $k[31])
}
is-deeply clamp(), (0, 96), 'ed25519-style clamping of a typed local buffer';
