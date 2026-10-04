use Test;
use lib $?FILE.IO.parent(2).add('lib');

# A block's phasers are split off its body before the body runs; a body
# without such phasers is run as written (the module mainline case).

plan 4;

use BlockPhasersSplit;

is phaser-log(), 'body,leave', 'a LEAVE in a module mainline runs after its body';

sub checked($x) {
    PRE { $x > 0 }
    POST { $x > 0 }
    $x * 2
}
is checked(3), 6, 'PRE/POST pass on a routine body';
throws-like { checked(-1) }, X::Phaser::PrePost, 'a failing PRE throws';

my @seen;
{
    @seen.push('a');
    @seen.push('b');
}
is @seen.join(','), 'a,b', 'a phaser-free block runs its statements in order';
