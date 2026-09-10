use Test;

# Raku installs an END phaser as its compiler walks past it, so ENDs run in
# reverse SOURCE order — a mainline END and one inside a block are not two
# classes. mutsu hoists the mainline's top-level ENDs (so they still run when
# the body dies) and used to sort every hoisted one ahead of every block-scoped
# one, which reversed the wrong pair:
#
#   { END { say "END1" } } { END { say "END2" } } END { say "END3" }
#   raku : END3 END2 END1     mutsu: END2 END1 END3
#
# Each case runs in a child process, since an END's whole point is that it runs
# at exit, and each program is written across several LINES: mutsu orders a
# compunit's ENDs by source line, so several ENDs on one physical line still
# tie (see todo/deep/end-phasers-install-at-compile-time.md).

use lib $?FILE.IO.parent(2).add('roast/packages/Test-Helpers/lib').Str;
use Test::Util;

plan 5;

is_run qq:to/CODE/,
    END \{ say 3 }
    \{ END \{ say 2 } }
    CODE
    { out => "2\n3\n", err => '', status => 0 },
    'a block END written after a mainline END runs first';

is_run qq:to/CODE/,
    \{ END \{ say 1 } }
    \{ END \{ say 2 } }
    END \{ say 3 }
    CODE
    { out => "3\n2\n1\n", err => '', status => 0 },
    'the mainline END wins even when written last';

is_run qq:to/CODE/,
    END \{ say "A" }
    say "body";
    END \{ say "B" }
    sub f \{ END \{ say "C" } }
    f();
    \{ END \{ say "D" } }
    CODE
    { out => "body\nD\nC\nB\nA\n", err => '', status => 0 },
    'mainline, sub and block ENDs interleave in one reverse-source sequence';

is_run qq:to/CODE/,
    \{ END \{ say 1 } }
    END \{ say 2 }
    \{ END \{ say 3 } }
    CODE
    { out => "3\n2\n1\n", err => '', status => 0 },
    'a mainline END between two block ENDs keeps its source position';

# The eager hoist this ordering had to preserve: an END still runs when the
# mainline dies before reaching it.
is_run 'say "start"; die "boom"; END { say "after-die" }',
    { out => "start\nafter-die\n", status => 1 },
    'a mainline END still runs when the body dies before reaching it';
