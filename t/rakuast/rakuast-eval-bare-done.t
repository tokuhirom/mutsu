use v6;
use Test;

# A bare `done` is a core routine, not an undeclared name: an EVAL string with
# `done` inside `react` must compile whichever frontend lowers it, and outside
# `react`/`supply` it fails at run time, not at compile time.
plan 4;

my $code = 'sub g($s) { react { whenever $s -> $v { 1 }; done; } }; 42';
is EVAL($code), 42, 'react with a bare done compiles';

my $do = 'sub h($s) { react { my $t = do whenever $s -> $v { 1 }; done; } }; 7';
is EVAL($do), 7, 'react with do whenever and a bare done compiles';

my $threw = False;
try { EVAL 'done'; CATCH { default { $threw = .message ~~ /'done without supply or react'/ } } }
ok $threw, 'a bare done outside react fails at run time';

is EVAL('my $r = ""; react { whenever Supply.from-list(1, 2) { $r ~= $_; done if $_ == 2 } }; $r'), '12',
    'a bare done after a modifier still ends the react';
