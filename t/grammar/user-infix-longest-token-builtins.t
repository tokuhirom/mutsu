use Test;

# A user-declared single-character infix (`+`, `~`, `?`, `*`, `/`) must not
# claim the first half of a longer built-in infix token: longest-token matching
# picks `+&`, `+|=`, `~^`, `**`, `//`, ... Ecosystem EC declares
# `multi sub infix:<+>(Point, Point)` and then writes `$s[0] +&= 0b1111_1000`.

plan 12;

class P { has $.v }
multi sub infix:<+>(P $a, P $b) { P.new(v => $a.v + $b.v) }
multi sub infix:<~>(P $a, P $b) { P.new(v => $a.v ~ $b.v) }
multi sub infix:<?>(P $a, P $b) { P.new(v => 'q') }
multi sub infix:<*>(P $a, P $b) { P.new(v => $a.v * $b.v) }
multi sub infix:</>(P $a, P $b) { P.new(v => $a.v / $b.v) }

my @s = 255, 255, 1;
@s[0] +&= 0b1111_1000;
is @s[0], 248, '+&= after a subscript';
@s[*-1] +|= 6;
is @s[*-1], 7, '+|= after a whatever subscript';
@s[1] +^= 15;
is @s[1], 240, '+^= after a subscript';
is 1 +< 3, 8, '+< shift';
is 16 +> 2, 4, '+> shift';
is 'a' ~| 'b', 'c', '~| string or';
is 2 ** 3, 8, '** power';
is Nil // 5, 5, '// defined-or';
is ?(True ?| False), True, '?| boolean or';
ok 'abc' ~~ /b/, '~~ smartmatch';
is (P.new(v => 1) + P.new(v => 2)).v, 3, 'the user infix:<+> itself still works';
is (P.new(v => 2) * P.new(v => 3)).v, 6, 'the user infix:<*> itself still works';
