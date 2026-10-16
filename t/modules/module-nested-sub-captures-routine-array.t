use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;
use NestedSubRoutineArray;

# Helper subs nested in a module routine reach the routine's own typed `@`
# variables (a native `my str @out`, an object-typed `my Str @log`), never a
# same-named variable of the script that calls the routine. The routine's
# variables take the cell the nested subs capture (ADR-12529 §3, slice 32;
# the JSON::Fast `to-json` shape). Checked against rakudo.

plan 3;

my @out = 'q', 'r';
my @log = 'x';
is render('v'), '{v}|v', 'the nested subs write the routine\'s own arrays';
is render('w'), '{w}|w', 'and fresh ones on the next call';
is-deeply (@out, @log), (['q', 'r'], ['x']), 'the caller\'s same-named arrays are untouched';
