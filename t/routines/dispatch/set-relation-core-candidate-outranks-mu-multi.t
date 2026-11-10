use Test;
plan 8;
my @l = 1, 2;
my @r = 1, 2, 3;
{
    multi sub infix:<⊆>(Mu $a, Mu $b) { 'user' }
    multi sub infix:<⊇>(Mu $a, Mu $b) { 'user' }
    multi sub infix:<⊂>(Mu $a, Mu $b) { 'user' }
    multi sub infix:<⊃>(Mu $a, Mu $b) { 'user' }
    is-deeply (@l ⊆ @r), True, '⊆ keeps the core (Any, Any) candidate over a (Mu, Mu) multi';
    is-deeply (@r ⊇ @l), True, '⊇';
    is-deeply (@l ⊂ @r), True, '⊂';
    is-deeply (@r ⊃ @l), True, '⊃';
}
{
    multi sub infix:<⊆>(Int $a, Int $b) { 'user' }
    is (1 ⊆ 2), 'user', 'a narrower (Int, Int) multi still wins';
}
{
    multi sub infix:<⊆>(Positional $a, Positional $b) { 'user' }
    is (@l ⊆ @r), 'user', 'a (Positional, Positional) multi is narrower than (Any, Any)';
}
is-deeply (@l ⊆ @r), True, 'no user candidate in scope';
is-deeply (<a b>.Set ⊆ <a b c>.Set), True, 'sets';
