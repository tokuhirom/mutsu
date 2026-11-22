use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A statement modifier after a block-form phaser (`BEGIN { ... } for ^3`)
# governs only the statement's value: its topic or condition still runs, and
# the phaser is installed unconditionally and runs as it always does (#18148).
# The bare form (`BEGIN say "x" for ^2`) puts the modifier on the phaser's own
# statement instead. Expected outputs are rakudo 2026.11's.

plan 12;

is_run ｢BEGIN { say "once" } for ^3;｣, %(:out("once\n"), :err('')),
    'BEGIN block, then a for modifier: the body runs once';
is_run ｢BEGIN { say "once" } if 0;｣, %(:out("once\n"), :err('')),
    'a false if modifier does not stop the BEGIN';
is_run ｢my $x; BEGIN { $x = 1; say "once" } for ^3; say $x｣,
    %(:out("once\n1\n"), :err('')), 'the BEGIN result is visible afterwards';
is_run ｢sub f { BEGIN { say "in-f" } for ^2; 1 }; f()｣, %(:out("in-f\n"), :err('')),
    'inside a routine body';
is_run ｢CHECK { say "check" } for ^2;｣, %(:out("check\n"), :err('')), 'CHECK';
is_run ｢BEGIN { say "b" } for (say("topic"), 2);｣, %(:out("b\ntopic\n"), :err('')),
    'the modifier topic is still evaluated, at run time';
is_run ｢for ^2 { FIRST { say "first" } if 1; }｣, %(:out("first\n"), :err('')),
    'FIRST stays a loop phaser: once per loop';
is_run ｢for ^2 { FIRST { say "first" } if 0; say "body" }｣,
    %(:out("first\nbody\nbody\n"), :err('')), '... and is installed whatever the condition';
is_run ｢sub g { LEAVE { say "leave" } if 0; say "g" }; g()｣, %(:out("g\nleave\n"), :err('')),
    'LEAVE is installed whatever the condition';
is_run ｢for ^3 { NEXT { say "next" } unless 1; }｣, %(:out("next\n" x 3), :err('')),
    'NEXT under unless';
is_run ｢BEGIN { say "b" }
for ^2 { say "loop" }｣, %(:out("b\nloop\nloop\n"), :err('')),
    'a for loop on the next line is a statement of its own';
is_run ｢BEGIN say "x" for ^2;｣, %(:out("x\nx\n"), :err('')),
    'the bare form modifies the phaser statement itself';
