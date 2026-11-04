use lib 'roast/packages/Test-Helpers/lib';
use Test;

# A closure call refreshes the captured env of END phasers for the variables
# the closure may have written. That refresh walks the closure's capture; it
# must still reach a plain lexical, a loop-carried counter and a dynamic
# variable written by a closure created inside a block, with a `use` in the
# unit (so the capture holds shared system-name layers).

use Test::Util;

plan 6;

my $log = '';
my $count = 0;
END { $log ~= "end:$count"; }

my &bump = -> { $count++ };
bump() for ^3;
is $count, 3, 'a closure writes a captured plain lexical';

my $sum = 0;
for 1..4 -> $i {
    my &add = -> $n { $sum += $n * $i };
    add($_) for 1..2;
}
is $sum, 30, 'closures created in a loop write a captured lexical';

sub with-dynamic {
    my $*depth = 1;
    my &deeper = -> { $*depth++ };
    deeper() for ^2;
    $*depth
}
is with-dynamic(), 3, 'a closure writes a dynamic variable of the creating frame';

my class Point { has $.x = 1 }
my $made = 0;
my &make = -> { $made++; Point.new };
make() for ^3;
is $made, 3, 'a closure that builds an object still counts through its capture';

is_run 'my $n = 0; END { say "n=$n" }; my &f = -> { $n += 2 }; f() for ^3;',
    { out => "n=6\n" }, 'an END phaser reads the value a closure wrote';

is_run 'my $s = ""; END { say "s=$s" }; for <a b> -> $x { my &g = -> { $s ~= $x }; g() }',
    { out => "s=ab\n" }, 'an END phaser sees writes of closures created in a loop';
