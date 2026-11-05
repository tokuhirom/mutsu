use Test;
use lib 't/lib';

# Every built-in operator of a closure in a unit with a `use` carries a
# lexical-import prefix (ADR-8751 stage 2). A trivial closure still takes the
# light call while no import binds an operator (#9170), and must keep the
# imported operator once a block's `use` binds one.

plan 8;

use JSON::Fast;

my $plain = -> $x, $y { $x + $y };
is $plain(1, 2), 3, 'a closure under a unit-level use adds';

my $sum = 0;
$sum += $plain($_, 1) for ^100;
is $sum, 5050, 'called in a loop';

my $wc = * * 2;
is $wc(21), 42, 'a WhateverCode under a unit-level use';

my $ph = { $^a ~ $^b };
is $ph('a', 'b'), 'ab', 'a placeholder block under a unit-level use';

{
    use LightClosureOpMod;
    my $inner = -> $x, $y { $x + $y };
    my $m = $inner(Money.new(n => 1), Money.new(n => 2));
    is $m.amount, 3, "a block's use binds the operator for its own closure";
    is $inner(3, 4), 7, '... which still adds plain numbers';
}

is $plain(5, 6), 11, 'the outer closure is unaffected after the block';
is $plain(1, 2), 3, 'and so is a repeat call';
