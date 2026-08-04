use v6;
use lib 't/lib';
use Test;
use BeginOnceInRoutine;

plan 5;

# `BEGIN <expr>` promises ONE evaluation. The phaser lifter hoists a BEGIN out
# of the mainline, but it never walks a module's routine bodies, so every BEGIN
# inside a module was compiled inline and re-evaluated on every execution --
# `Digest::SHA2` rebuilt its 64-word round-constant table once per round.

is direct(), 1, 'a BEGIN inside a module routine yields its value';
is direct(), 2, 'the second call pushes into the SAME array';
is direct(), 3, 'and so does the third -- the BEGIN body ran once';

# The `reduce` callback is recompiled on every iteration (the AST carrier), so
# the memo cell must not be keyed by the compilation that emitted it.
is via-reduce(), 4, 'a BEGIN inside a reduce callback is one array for all 4 iterations';

is twice(), False, 'two identical BEGINs on one line are separate sites';
