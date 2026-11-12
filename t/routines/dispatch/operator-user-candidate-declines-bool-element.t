use Test;

# A user `infix:<==>` (and kin) candidate in scope routes the operator through
# the generic infix call, which hands a subscript operand over as its
# element's container. When no user candidate accepts the call, the core
# numeric comparison must still read a Bool stored in a Hash element as 1/0
# (#17513: Red's migration planner judged equal `auto-increment` columns
# different).

multi infix:<==>(Str $a, Str $b) { 'user ==' }
multi infix:<!=>(Str $a, Str $b) { 'user !=' }
multi infix:«<»(Str $a, Str $b)  { 'user <' }
multi infix:«>=»(Str $a, Str $b) { 'user >=' }

plan 12;

my %h = t => True, f => False;

is-deeply (1 == %h<t>), True,  'Int == Bool hash element';
is-deeply (%h<t> == 1), True,  'Bool hash element == Int';
is-deeply (0 == %h<f>), True,  'Int == False hash element';
is-deeply (1 != %h<t>), False, 'Int != Bool hash element';
is-deeply (%h<t> != %h<t>), False, 'two Bool hash elements !=';
is-deeply (0 < %h<t>), True,   'Int < Bool hash element';
is-deeply (%h<t> >= 1), True,  'Bool hash element >= Int';

my %nested = cols => %( id => %( pk => True ) );
is-deeply (1 == %nested<cols><id><pk>), True, 'nested Bool hash element';

my $hash = %h;
is-deeply (1 == $hash<t>), True, 'Bool element of an itemized Hash';

my @a = True;
is-deeply (1 == @a[0]), True, 'Bool array element';

is ('a' == 'a'), 'user ==', 'the user candidate still takes the calls it accepts';
is ('a' != 'b'), 'user !=', 'the user != candidate too';
