use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 6;

# `note(...)` with the parenthesis attached is an ordinary call. mutsu's
# `note` statement parser tolerated a missing argument list, so it matched the
# no-argument form and left `("hi")` behind as a separate statement: the program
# printed "Noted" and warned about a string in sink context.

is_run 'note("hi")',
    { out => '', err => "hi\n", status => 0 },
    'note(ARG) writes its argument to stderr';

is_run 'my $x = "a"; note($x)',
    { out => '', err => "a\n", status => 0 },
    'note(VAR) writes the variable to stderr';

is_run 'note "hi"',
    { out => '', err => "hi\n", status => 0 },
    'the listop form is unchanged';

is_run 'note',
    { out => '', err => "Noted\n", status => 0 },
    'bare note still prints Noted';

is_run 'note()',
    { out => '', err => "Noted\n", status => 0 },
    'note() with an empty argument list prints Noted';

is_run 'note ("a", "b")',
    { out => '', err => "(a b)\n", status => 0 },
    'a space before the paren still passes one List argument';
