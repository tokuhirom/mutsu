use Test;
plan 12;
my @l = 1, 2; my @r = 2, 3;
multi sub infix:<∖>(Mu $a, Mu $b) { 'user' }
multi sub infix:<∪>(Mu $a, Mu $b) { 'user' }
multi sub infix:<∩>(Mu $a, Mu $b) { 'user' }
multi sub infix:<⊖>(Mu $a, Mu $b) { 'user' }
multi sub infix:<⊍>(Mu $a, Mu $b) { 'user' }
multi sub infix:<⊎>(Mu $a, Mu $b) { 'user' }
is-deeply (@l ∖ @r), set(1), '∖ keeps the core candidate';
is-deeply (@l ∪ @r), set(1, 2, 3), '∪';
is-deeply (@l ∩ @r), set(2), '∩';
is-deeply (@l ⊖ @r), set(1, 3), '⊖';
is (@l ⊍ @r).elems, 1, '⊍';
is-deeply (@l ⊎ @r), bag(1, 2, 2, 3), '⊎';
is-deeply (@l (-) @r), set(1), '(-) spelling';
is-deeply (@l (|) @r), set(1, 2, 3), '(|) spelling';
{
    multi sub infix:<∖>(Positional $a, Positional $b) { 'narrow' }
    is (@l ∖ @r), 'narrow', 'a narrower candidate still wins';
}
my %h = a => 1;
{
    multi sub infix:<∪>(Int $a, Int $b) { 'ints' }
    is (1 ∪ 2), 'ints', 'an (Int, Int) candidate wins for Ints';
}
is-deeply ((1, 2) ∖ (2,)), set(1), 'list operands';
is-deeply (<a b>.Set ∪ <c>.Set), set(<a b c>), 'sets';
