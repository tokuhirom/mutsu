use Test;
plan 3;

my $x = 0;
repeat { $x++; } while $x < 3;
is $x, 3, "repeat while";

my $y = 5;
repeat { $y++; } until $y > 7;
is $y, 8, "repeat until";

my $z = 0;
my $first = True;
repeat until $z >= 3 -> $bound {
    if $first {
        ok !$bound.defined, "repeat until pointy binding starts undefined";
        $first = False;
    }
    $z++;
}
