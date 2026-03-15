use Test;

plan 15;

# NaN comparisons: all ordering comparisons return False
nok NaN < 1, 'NaN < 1 is False';
nok NaN > 1, 'NaN > 1 is False';
nok NaN <= 1, 'NaN <= 1 is False';
nok NaN >= 1, 'NaN >= 1 is False';
nok 1 <= NaN, '1 <= NaN is False';
nok 1 >= NaN, '1 >= NaN is False';
nok NaN == NaN, 'NaN == NaN is False';
nok NaN == 1, 'NaN == 1 is False';

# Approximate equality
ok Inf =~= Inf, 'Inf =~= Inf is True';
ok -Inf =~= -Inf, '-Inf =~= -Inf is True';
nok NaN =~= NaN, 'NaN =~= NaN is False';

# $*TOLERANCE dynamic variable
{
    my $*TOLERANCE = 0.1;
    ok 1.01 =~= 1, '1.01 =~= 1 with tolerance 0.1';
    ok 0.99 =~= 1, '0.99 =~= 1 with tolerance 0.1';
    nok 0.00120 =~= 0.001, '0.00120 not =~= 0.001 with tolerance 0.1 (relative)';
    nok 0.00080 =~= 0.001, '0.00080 not =~= 0.001 with tolerance 0.1 (relative)';
}
