use Test;

# `run(..., :env(%*ENV, |%extra))` parses `:env`'s argument as a List (a Hash
# followed by a Slip of Pairs), not a literal Hash. Rakudo coerces it with
# hash semantics (`.hash`) before building the child environment, layering
# the extra pairs over %*ENV; mutsu silently dropped the extra pairs, using
# only the leading Hash (issue #9085).

plan 2;

my $p = run(
    $*EXECUTABLE, '-e', 'say %*ENV<XX_RUN_ENV_LIST_TEST> // "none"',
    :out, :env(%*ENV, |{ XX_RUN_ENV_LIST_TEST => '1' }),
);
is $p.out.slurp(:close).trim, '1',
    'run(:env(%*ENV, |%extra)) coerces the List to a Hash instead of dropping the extra pairs';

# A plain Hash :env still works (the common case this must not regress).
my $p2 = run(
    $*EXECUTABLE, '-e', 'say %*ENV<XX_RUN_ENV_HASH_TEST> // "none"',
    :out, :env(%(XX_RUN_ENV_HASH_TEST => '2')),
);
is $p2.out.slurp(:close).trim, '2', 'run(:env(%hash)) still works';
