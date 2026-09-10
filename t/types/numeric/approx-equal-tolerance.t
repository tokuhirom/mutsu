use v6;
use Test;

# The approximately-equal operator `≅` / `=~=` uses a *strict* relative-difference
# test against $*TOLERANCE (default 1e-15), which is dynamically overridable.
# Regressions fixed:
#  - `1 ≅ 1` short-circuited to True even with $*TOLERANCE = 0
#  - the relative comparison used `<=` instead of strict `<`
#  - built-in dynamics tripped X::Dynamic::Postdeclaration on read-then-shadow

plan 14;

# $*TOLERANCE = 0 makes every finite comparison fail (no a==b short-circuit).
{
    my $*TOLERANCE = 0;
    nok 1 ≅ 1, '1 ≅ 1 is False when $*TOLERANCE = 0';
    nok 0 ≅ 0, '0 ≅ 0 is False when $*TOLERANCE = 0';
    ok Inf ≅ Inf, 'Inf ≅ Inf is True even when $*TOLERANCE = 0';
}

# A lexically-widened tolerance.
{
    my $*TOLERANCE = .1;
    ok 11 ≅ 10, '11 ≅ 10 is True when $*TOLERANCE = .1';
    nok 12 ≅ 10, '12 ≅ 10 is False when $*TOLERANCE = .1';
}

# The not-arithmetically-symmetric behaviour, using an explicit tolerance.
{
    my $*TOLERANCE = 1e-15;
    my $x = 1;
    nok ($x + $*TOLERANCE) ≅ $x, '($x + $*TOLERANCE) ≅ $x is False';
    ok  ($x - $*TOLERANCE) ≅ $x, '($x - $*TOLERANCE) ≅ $x is True';
}

# Default-tolerance behaviour (the operator falls back to 1e-15).
ok 1 ≅ 1,            '1 ≅ 1 is True at the default tolerance';
ok 0 ≅ 0,            '0 ≅ 0 is True at the default tolerance';
ok Inf ≅ Inf,        'Inf ≅ Inf';
nok Inf ≅ -Inf,      'Inf ≅ -Inf is False';
ok 1 =~= 1.0000000000000001, 'ASCII =~= within tolerance';
nok 100 =~= 101,     '100 =~= 101 is False';

# A built-in dynamic variable may be read and then lexically shadowed with `my`
# without tripping X::Dynamic::Postdeclaration (unlike a user dynamic).
{
    my $ok = $*OUT.defined;
    { my $*OUT; }
    ok $ok, 'reading then my-shadowing a built-in dynamic ($*OUT) is allowed';
}
