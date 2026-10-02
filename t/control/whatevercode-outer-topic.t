use Test;

# A WhateverCode whose body mentions the outer `$_` must not name its own
# parameter `$_`, or that mention would see the curried argument instead.
# The "mentions `$_`" check walks the whole curried expression (ADR-0137
# visitor), so a `$_` in a call argument or a string interpolation counts.
# Checked against rakudo.

plan 4;

sub id($x) { $x }

$_ = 10;
is (* + id($_))(1), 11, 'a $_ in a sub call argument is the outer topic';
is (* ~ "$_")(1), '110', 'a $_ in a string interpolation is the outer topic';
is (* + $_)(1), 11, 'a bare $_ operand is the outer topic';
is (1, 2).map(*.map({ $_ * 2 }).sum).sum, 6,
    'a $_ inside a nested block is that block\'s own topic';
