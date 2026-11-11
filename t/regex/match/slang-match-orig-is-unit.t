use lib 't/lib';
use Test;

# A slang production's cursor has the whole compilation unit as its `.orig`
# and its own position as `.from`, so an action can place what it matched
# (#16888, EEL 0.3's "definition records source line").

plan 3;

use HereSlang;

is here-pos, '12:4', 'the line and column of the production';
my $x = 1; is here-pos, '13:15', 'a later position on another line';
is orig-chars, $*PROGRAM.slurp.chars, '.orig is the whole unit, not the rest of it';
