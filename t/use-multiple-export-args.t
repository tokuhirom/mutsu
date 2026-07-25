use Test;
use lib 't/lib';

plan 8;

# A `use` line may carry a comma-separated argument list; each element is a
# separate positional argument to the module's `sub EXPORT`.
# (e.g. `use RakudoPrereq v2021.04, 'too old', 'rakudo-only'`)

use UseArgsFixture 1, 'two', 3.5;
is-deeply use-args().List, (1, 'two', 3.5), 'three `use` arguments reach sub EXPORT';

# The other argument shapes run in a subprocess: a module's `sub EXPORT` runs
# once per process, so re-`use`ing the same fixture in this file would not
# re-record.
sub args-for($use-line) {
    my $proc = run :out, :err, $*EXECUTABLE, '-I', 't/lib',
        '-e', "$use-line say use-args().raku";
    my $out = $proc.out.slurp-rest(:close).chomp;
    $proc.err.slurp-rest(:close);
    $out;
}

is args-for('use UseArgsFixture "a", "b";'), '["a", "b"]',
    'a comma list of two arguments';
is args-for('use UseArgsFixture "only";'), '["only"]',
    'a single argument still works';
is args-for('use UseArgsFixture;'), '[]',
    'no arguments still works';
is args-for('use UseArgsFixture <a b c>;'), '["a", "b", "c"]',
    'a word list still flattens into positionals';
is args-for('use UseArgsFixture 1, 2, 3, 4, 5;'), '[1, 2, 3, 4, 5]',
    'five arguments all arrive';

# `use lib` takes a *list* of repository specs, not one stringified blob.
{
    my $dir = 't/lib';
    lives-ok { EVAL "use lib 'no/such/dir', '$dir'; use UseArgsFixture 'x'" },
        '`use lib` with a comma list registers every path';
    lives-ok { EVAL "use lib <no/such/dir $dir>; use UseArgsFixture 'y'" },
        '`use lib` with a word list registers every path';
}

# vim: expandtab shiftwidth=4
