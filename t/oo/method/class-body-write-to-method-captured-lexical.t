use Test;

# A class-body statement (`BEGIN`, a bare assignment) that writes a lexical of
# the enclosing scope must not lose the write when a method of the same class
# reads that lexical. The method-capture pass at class registration used to box
# and snapshot the frame's slot before the body's by-name env write reached it
# (#10751).
plan 12;

my $cell;
class A { BEGIN { $cell = 5 }; method n { $cell } }
is A.n, 5, 'BEGIN write is seen by a method of the class';
is $cell, 5, 'BEGIN write is seen by the enclosing scope';

my $t;
{
    class E { BEGIN { $t = 4 }; method n { $t } }
    is E.n, 4, 'class in a nested block: method sees the BEGIN write';
}
is $t, 4, 'class in a nested block: enclosing scope sees the BEGIN write';

sub f {
    my $x = 1;
    class F { $x = 11; method n { $x } }
    my @r = F.n, $x;
    $x = 12;
    @r.push: F.n;
    @r
}
my @r = f();
is @r[0], 11, 'class body assignment in a routine: method sees it';
is @r[1], 11, 'class body assignment in a routine: routine sees it';
is @r[2], 12, 'method and routine still share one container afterwards';

my $w;
class L { BEGIN { $w = 1 }; method inc { $w++ } }
L.inc; L.inc;
is $w, 3, 'method write lands on the BEGIN-initialised lexical';
$w = 10;
L.inc;
is $w, 11, 'later outer write is seen by the method';

# A `my` in a nested block of the class body is that block's own variable: it
# must neither shadow nor overwrite the enclosing scope's lexical.
my $p = 1;
class N { { my $p = 7; }; method n { $p } }
is N.n, 1, 'nested-block `my` in a class body does not reach the method';
is $p, 1, 'nested-block `my` in a class body leaves the outer lexical alone';
my $q = 2;
class N2 { { my $q = 8; } }
is $q, 2, 'same without any method reading the lexical';
