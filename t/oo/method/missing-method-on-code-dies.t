use Test;

# A method a Block/Sub does not have is X::Method::NotFound (#11445). Only a
# WhateverCode *expression* curries a method call, and the compiler does that.

plan 11;

my $block = { $_ };
throws-like { $block.abs }, X::Method::NotFound,
    :method<abs>, :typename<Block>, 'missing method on a Block dies';

my $sub = sub ($x) { $x };
throws-like { $sub.abs }, X::Method::NotFound,
    :method<abs>, :typename<Sub>, 'missing method on a Sub dies';

my $wc = * - 5;
throws-like { $wc.abs }, X::Method::NotFound,
    :method<abs>, :typename<WhateverCode>, 'missing method on a WhateverCode value dies';

is (* - 5).abs.(2), 3, 'a method on a WhateverCode expression curries';
is (* - *).abs.(1, 5), 4, 'the curried method keeps the arity of the expression';
is (*.uc).lc.('aB'), 'ab', 'a method chain on a parenthesized *.method curries';
is (* + 1).Str.chars.(99), 3, 'a longer chain curries';

is $block.arity(:zzz), 0, 'an undeclared named does not make .arity miss';
is $block.?abs, Nil, '.? on a missing method of a Block is Nil';
is (sub {}).?native_call_convention, Nil, '.? on a missing method of a Sub is Nil';
is $block.?count, 1, '.? on an existing method still calls it';
