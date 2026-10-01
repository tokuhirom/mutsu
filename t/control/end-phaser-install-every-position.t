use Test;

# The compile-time END pre-installation walks the compunit with the typed AST
# visitor (ADR-0137), so an END in a never-run position that an older
# hand-rolled walker did not list is still installed and runs at exit, as in
# rakudo. Expected outputs were measured against rakudo 2026.07.

use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

plan 3;

is_run 'END { say "main" }; if False { say "x{ END { say "interp" } }" }',
    { out => "interp\nmain\n", err => '', status => 0 },
    'an END inside an interpolated block of a never-run statement';

is_run 'END { say "main" }; sub f($x = { END { say "default" } }) { }',
    { out => "default\nmain\n", err => '', status => 0 },
    'an END inside a parameter default of an uncalled sub';

# `loop (my $i ...)` declares `$i` in the enclosing scope, so a never-reached
# END in the body reads it as an unassigned container, not its live value.
is_run 'END { say "main" }; loop (my $i = 0; $i < 0; $i++) { END { say "loop {$i.raku}" } }',
    { out => "loop Any\nmain\n", err => '', status => 0 },
    'a never-reached END sees a loop-init variable as unassigned';
