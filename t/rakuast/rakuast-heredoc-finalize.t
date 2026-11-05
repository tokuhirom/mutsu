use v6;
use Test;

plan 4;

# An interpolating heredoc is carried through finalization as the parser built
# it (ADR-15760 slice 2); its body is read as a `qq` string when the unit compiles.

my $w = 'x';
my $t = qq:to/E/;
    hi $w {1 + 1}
    E
is $t, "hi x 2\n", 'variables and blocks interpolate';

sub f { my $z = 4; my $r = qq:to/E/;
    v=$z
    E
    $r }
is f(), "v=4\n", 'a heredoc in a routine sees the routine lexicals';

my @l = <a b>;
is qq:to/E/, "@l[] @l[0]\n", 'array interpolation';
    @l[] @l[0]
    E

is q:to/E/, "no \$w\n", 'q:to does not interpolate';
    no $w
    E
