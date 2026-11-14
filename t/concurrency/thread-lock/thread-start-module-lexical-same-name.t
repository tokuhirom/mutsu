use v6;
use Test;

# A `start` run from a module routine two calls deep must leave the caller
# frames' lexicals alone. Spawning used to flatten the parent's scoped env in
# place, which merged a module routine's `my Int $n = 0` with the script's
# same-named `$n`, so the routine read `Int` back (Rakuish's agent server,
# #17747). Same as raku.

plan 2;

my $p = run $*EXECUTABLE, '-I', 't/lib-spawn-scoped-env', '-e',
    'use SpawnB; await start { 1 }; my $n = SpawnB.new.run; say $n',
    :out, :err;
is $p.out.slurp(:close), "2\n", 'the routine keeps its own counter';
is $p.err.slurp(:close), '', 'no warning about an uninitialized Int';
