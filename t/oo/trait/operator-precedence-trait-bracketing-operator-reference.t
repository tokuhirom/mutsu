use Test;

# From Math::Matrix: `is tighter(&postcircumfix:<[ ]>)`. The name of a bracketing
# operator holds a space, which the RakuAST round-trip refused as a precedence
# reference, so the whole module could not load.
plan 4;

multi sub prefix:<MM>(Str $m) is tighter(&postcircumfix:<[ ]>) { "MM($m)" }
is (MM "a"), 'MM(a)', 'tighter than a postcircumfix';

sub infix:<zz>(Str $a, Str $b) is equiv(&circumfix:<[ ]>) { $a ~ '-' ~ $b }
is ("a" zz "b"), 'a-b', 'equiv to a circumfix';

sub infix:<yy>(Str $a, Str $b) is looser(&postcircumfix:<{ }>) { $a ~ '+' ~ $b }
is ("a" yy "b"), 'a+b', 'looser than a brace postcircumfix';

multi sub infix:<⋅>(Int $a, Int $b) is tighter(&infix:<*>) { $a * $b }
is (3 ⋅ 4), 12, 'a plain operator reference still works';

# vim: expandtab shiftwidth=4
