use Test;

plan 4;

my %ball = map {; $_ => 1 }, 1..12;
is(([+] %ball{11,12}) <=> ([+] %ball{1,2}), Order::Same, 'brace hash slice parses as comma-list index');

throws-like ｢say ’a‘ <=> ’b‘｣, X::Str::Numeric, '<=> throws for non-numeric strings';

is-deeply <1/0> <=> <-1/0>, More, 'Rat infinities compare by sign when denominator is zero';
is-deeply More, Order::More, 'bare Order variants are available';
