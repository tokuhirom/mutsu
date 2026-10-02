use Test;

# ADR-0120: a strand result that is never read never allocates its characters.
# Binding one to a variable used to break that: every env insert probed the
# value with `as_str()` (for the sigilless-alias index) before checking the
# key, which flattened `"a" x 2**32 - 1` into a 4 GiB buffer -- 4 seconds and
# 4 GiB RSS for a line that reads nothing. Each child runs under a 1 GiB
# address-space limit, so a materialized result cannot fit.

plan 6;

my $exe = $*EXECUTABLE.absolute;

sub alive-under-limit(Str $code) {
    my $p = run 'bash', '-c', 'ulimit -v 1000000; exec "$@"', '_', $exe, '-e', $code,
        :out, :err;
    my $out = $p.out.slurp(:close);
    $p.err.slurp(:close);
    $out.trim
}

is alive-under-limit('my $n = "a" x 2**32 - 1; say "alive"'), 'alive',
    'my $ binding does not materialize a strand result';
is alive-under-limit('my str $n = "a" x 2**32 - 1; say "alive"'), 'alive',
    'my str binding does not materialize a strand result';
is alive-under-limit('our $n = "a" x 2**32 - 1; say "alive"'), 'alive',
    'our binding does not materialize a strand result';
is alive-under-limit('my %h = k => "a" x 2**32 - 1; say "alive"'), 'alive',
    'a hash value does not materialize a strand result';
is alive-under-limit('my @a; @a[0] = "a" x 2**32 - 1; say "alive"'), 'alive',
    'an array element does not materialize a strand result';
is alive-under-limit('my $n = "ab" x 2**20; say $n.chars'), 2**21,
    'a bound strand result still reads back right';
