use Test;

sub infix:<+%>($a, $b) { "m($a,$b)" }
sub infix:<%%%>($a, $b) { "p($a,$b)" }
sub infix:<+++>($a, $b) { "q($a,$b)" }
sub infix:<< <> >>($a, $b) { "n($a,$b)" }

is ((1, 2) X+% (3, 4)), 'm(1,3) m(1,4) m(2,3) m(2,4)',
    'X takes the longest user infix extending a built-in prefix';
is ((1, 2) Z+% (3, 4)), 'm(1,3) m(2,4)',
    'Z takes the longest user infix extending a built-in prefix';
is ((1, 2) R+% (3, 4)), 'm(3 4,1 2)',
    'R takes the longest user infix extending a built-in prefix';
is ((1, 2) X%%% (3, 4)), 'p(1,3) p(1,4) p(2,3) p(2,4)',
    'a repeated symbolic user infix wins over its shorter built-in prefix';
is ((1, 2) X+++ (3, 4)), 'q(1,3) q(1,4) q(2,3) q(2,4)',
    'a user infix longer than the built-in increment token is consumed whole';
is ((1, 2) X<> (3, 4)), 'n(1,3) n(1,4) n(2,3) n(2,4)',
    'a user infix beginning with the built-in less-than token is consumed whole';

done-testing;
