use Test;

# rakudo runs END phasers after the mainline's warning handler is gone, so a
# `warn` that nothing in the END body handles is swallowed: nothing reaches
# stderr, whether the warning is an explicit `warn` or an uninitialized-value
# warning, and whether the END was reached or not. A CONTROL handler the body
# installs itself still sees the warning, and a warning raised by the mainline
# is reported as usual.
#
# Every case runs in a child process, since an END's whole point is that it
# runs at exit. Expected outputs were measured against rakudo 2026.07.

use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

plan 9;

is_run 'END { warn "w1"; say "after" }',
    { out => "after\n", err => '', status => 0 },
    'an explicit warn in an END body is swallowed';

is_run 'my $u; END { say "u=$u" }',
    { out => "u=\n", err => '', status => 0 },
    'an uninitialized-value warning (string context) in a reached END is swallowed';

is_run 'my $u; END { say +$u }',
    { out => "0\n", err => '', status => 0 },
    'an uninitialized-value warning (numeric context) in a reached END is swallowed';

# A never-reached END reads the enclosing lexical as an unassigned container
# (see end-phaser-compile-time-install.t); stringifying it must not warn either.
is_run 'my $t = 5; if False { END { say "t=$t" } }',
    { out => "t=\n", err => '', status => 0 },
    'interpolating a seeded lexical in a never-reached END does not warn';

is_run 'my $t = 5; if False { END { say "t=" ~ $t; put $t; say $t.Str } }',
    { out => "t=\n\n\n", err => '', status => 0 },
    'and neither do ~, put and .Str on it';

is_run 'END { CONTROL { default { say "ctl: ", .message } }; warn "w3" }',
    { out => "ctl: w3\n", err => '', status => 0 },
    'a CONTROL handler inside the END body still sees the warning';

is_run 'END { try { warn "in-try" }; say "ok" }',
    { out => "ok\n", err => '', status => 0 },
    'a warn under try inside an END is swallowed';

is_run 'warn "main-warn"; END { say "end" }',
    { out => "end\n", err => /'main-warn'/, status => 0 },
    'a warning raised by the mainline is still reported';

# The suppression is scoped to the END phase: a module-less program whose END
# runs after a warning-producing mainline reports the mainline warning only.
is_run 'my $u; say "main=$u"; END { my $v; say "end=$v" }',
    { out => "main=\nend=\n", err => /'uninitialized value'/, status => 0 },
    'the END phase does not mute the mainline, only itself';
