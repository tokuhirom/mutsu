use Test;

# `$*TOLERANCE` is declared in rakudo's PROCESS:: with a default of 1e-15, and
# `infix:<=~=>` / `infix:<≅>` compare against it. mutsu never seeded the
# variable, so reading it gave Nil and the doc example
# (`($x + $*TOLERANCE) ≅ $x`) silently degenerated into `$x ≅ $x` — True where
# raku says False.

plan 20;

is $*TOLERANCE, 1e-15, '$*TOLERANCE defaults to 1e-15';
isa-ok $*TOLERANCE, Num, '$*TOLERANCE is a Num';

my $x = 1;
nok ($x + $*TOLERANCE) ≅ $x, '1 + $*TOLERANCE is not approximately 1';
ok  ($x - $*TOLERANCE) ≅ $x, '1 - $*TOLERANCE rounds back to 1';
nok ($x + $*TOLERANCE) =~= $x, 'the =~= ASCII spelling agrees';

nok 1.000001 ≅ 1, 'a difference far above the tolerance is not approximate';
nok 100 ≅ 100.00001, 'the comparison is relative, not absolute';
ok  1 ≅ 1, 'identical values are approximately equal';

# Zero, Inf and NaN.
ok  0 ≅ 0, '0 ≅ 0';
ok  0 ≅ 1e-30, 'near-zero uses an absolute comparison';
nok 0 ≅ 1, '0 is not approximately 1';
ok  Inf ≅ Inf, 'Inf ≅ Inf';
nok NaN ≅ NaN, 'NaN is never approximately equal';

# Complex and Rat operands.
ok  (1.0+0i) ≅ 1.0, 'a Complex with a zero imaginary part matches its Real';
nok (1.0+1.0i) ≅ 1.0, 'a Complex with a real imaginary part does not';
ok  1/2 ≅ 0.5, 'Rat operands coerce';

# It is a dynamic variable, so an inner scope may override it — and a sub
# called from that scope must see the override, not the process default.
{
    my $*TOLERANCE = 0.1;
    ok 1 ≅ 1.05, 'a lexically overridden $*TOLERANCE is honored';
    sub tol-in-sub { $*TOLERANCE }
    is tol-in-sub(), 0.1, 'a called sub sees the caller-scoped override';
}
is $*TOLERANCE, 1e-15, 'the override does not leak past its scope';

# `cmp-ok $a, '=~=', $b` resolves the real operator in rakudo, so it must
# observe the same dynamic variable rather than a private hard-coded tolerance.
{
    my $*TOLERANCE = 0.1;
    cmp-ok 1, '=~=', 1.05, 'cmp-ok =~= honors $*TOLERANCE too';
}
