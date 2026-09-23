use Test;

plan 2;

# Text::Calendar 0.1.6 has this shape: its public positional candidate has a
# defaulted year plus named options, alongside a named-only candidate used by
# the internal redispatch. Rakudo treats the two candidates as equally narrow
# for a no-argument call, so the earlier positional candidate must run.
proto calendar-year(|) {*}
multi sub calendar-year(
    $year is copy = Whatever,
    UInt :$per-row = 3,
    Bool :t(:$transposed) = False,
) {
    'positional default'
}
multi sub calendar-year(
    :$year,
    UInt :$per-row = 3,
    Bool :t(:$transposed) = False,
) {
    'named only'
}

is calendar-year(), 'positional default',
    'a defaulted positional candidate competes with an exact named-only candidate';
is calendar-year(year => 2024), 'named only',
    'the named-only candidate remains applicable for a supplied named argument';
