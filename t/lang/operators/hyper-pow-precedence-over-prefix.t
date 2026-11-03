use v6;
use Test;

# From MIDI::Make: `my constant @pow2 = ^256 »**» 2;` -- the hyper `**` has `**`'s
# precedence, tighter than a prefix operator, so it is `^(256 »**» 2)`.
plan 6;

is (^3 »**» 2).elems, 9, '^N »**» M is ^(N »**» M)';
is (^3 «**» 2).elems, 9, 'the «**» spelling too';
is (-2 »**» 2), -4, 'prefix minus binds looser than »**»';
is (?3 »**» 2), True, 'so does prefix ?';
is (+"3" »**» 2), 9, 'and prefix +';
is ((1, 2, 3) »**» 2), (1, 4, 9), 'a parenthesized list operand is unchanged';
