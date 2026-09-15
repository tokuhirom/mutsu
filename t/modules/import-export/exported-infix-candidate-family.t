use v6;
use Test;

# A custom EXPORT may return the actual candidate family of an operator. The
# imported value must retain those candidates so dispatch can select the
# module's typed implementation before falling back to core operators.

plan 1;

my $lib = $*TMPDIR.add("mutsu-exported-infix-{$*PID}");
$lib.mkdir;
LEAVE { try { .unlink for $lib.dir; $lib.rmdir } }

$lib.add('EOperators.rakumod').spurt(q:to/END/);
    class EOperator is export { has $.n }

    my multi sub infix:<cmp>(EOperator:D $a, EOperator:D $b) { $a.n cmp $b.n }
    my multi sub infix:<eqv>(EOperator:D $a, EOperator:D $b) { $a.n eqv $b.n }
    my multi sub infix:<==>(EOperator:D $a, EOperator:D $b) { $a.n == $b.n }
    my multi sub infix:<!=> (EOperator:D $a, EOperator:D $b) { $a.n != $b.n }
    my multi sub infix:«<» (EOperator:D $a, EOperator:D $b) { $a.n < $b.n }
    my multi sub infix:«<=» (EOperator:D $a, EOperator:D $b) { $a.n <= $b.n }
    my multi sub infix:«>» (EOperator:D $a, EOperator:D $b) { $a.n > $b.n }
    my multi sub infix:«>=» (EOperator:D $a, EOperator:D $b) { $a.n >= $b.n }

    my sub EXPORT() {
        Map.new:
            '&infix:<cmp>' => &infix:<cmp>,
            '&infix:<eqv>' => &infix:<eqv>,
            '&infix:<==>'  => &infix:<==>,
            '&infix:<!=>'  => &infix:<!=>,
            '&infix:«<»'   => &infix:«<»,
            '&infix:«<=»'  => &infix:«<=»,
            '&infix:«>»'   => &infix:«>»,
            '&infix:«>=»'  => &infix:«>=»,
    }
    END

my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e', q:to/CODE/, :out, :err);
    use EOperators;
    my $a = EOperator.new(n => 1);
    my $b = EOperator.new(n => 2);
    print ($a cmp $b).raku ~ '|';
    print ($a eqv $a) ~ '|';
    print ($a == $b) ~ '|';
    print ($a != $b) ~ '|';
    print ($a < $b) ~ '|';
    print ($a <= $b) ~ '|';
    print ($a > $b) ~ '|';
    print ($a >= $b);
    CODE
my $out = $proc.out.slurp;
$proc.err.slurp;
is $out, 'Order::Less|True|False|True|True|True|False|False',
    'custom operator candidates returned by EXPORT dispatch for every comparison';
