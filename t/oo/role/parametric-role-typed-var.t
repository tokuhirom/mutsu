use v6;
use Test;

plan 5;

role Cup[::Contents] { }
role Glass[::Contents] { }
role Tray[::ItemType] { }
class EggNog { }
class MulledWine { }

# An uninitialized parameterized-role typed variable keeps its type args
# (advent2009-day18)
my Cup of EggNog $mug;
is $mug.WHAT.raku, 'Cup[EggNog]', 'my Cup of EggNog $mug — .WHAT.raku';

my Glass[MulledWine] $glass;
is $glass.WHAT.raku, 'Glass[MulledWine]', 'bracket form declaration';

my Tray of Glass of MulledWine $valuable;
is $valuable.WHAT.raku, 'Tray[Glass[MulledWine]]', 'nested parameterization';

# Direct parameterization value renders without parens
is Cup[EggNog].raku, 'Cup[EggNog]', 'ParametricRole .raku';

# Plain typed variables unchanged
my Int $n;
is $n.WHAT.raku, 'Int', 'plain typed variable .WHAT unchanged';
