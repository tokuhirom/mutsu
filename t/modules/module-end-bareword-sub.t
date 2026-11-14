use v6;
use Test;

# A module's END block that names a later-declared sub of the module as a
# bare term (`END { $seen or finish }` above `sub finish`) calls it: the term
# is the sub, not the string "finish". Testo's exit status depends on this.
# Same as raku.

plan 4;

my $p = run $*EXECUTABLE, '-I', 't/lib-end-bareword-sub', '-e',
    'use EndBareword; say "body"', :err, :out;
is $p.out.slurp(:close), "body\n", 'the program body runs';
is $p.err.slurp(:close), "finish ran\n", 'END calls the later-declared sub';
is $p.exitcode, 3, 'its exit status is the program status';

$p = run $*EXECUTABLE, '-I', 't/lib-end-bareword-sub', '-e',
    'use EndBareword; mark()', :err, :out;
$p.out.slurp(:close); $p.err.slurp(:close);
is $p.exitcode, 0, 'END skips it when the flag is set';
