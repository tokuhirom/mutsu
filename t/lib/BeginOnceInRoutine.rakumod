unit module BeginOnceInRoutine;

# A module's routines are never walked by the phaser lifter (that pass only
# reorders the mainline), so these BEGINs reach the compiler in place -- the
# case `BeginOnceExpr` memoizes. Evaluated once, `BEGIN []` is ONE array that
# every call pushes into; re-evaluated, each call would get a fresh empty one.

sub direct() is export {
    my $a = BEGIN [];
    $a.push(1);
    $a.elems
}

# `reduce` runs its callback through the AST carrier, which recompiles the
# block on every iteration. The memo cell is keyed by source identity, not by
# the compilation that emitted it, so the BEGIN still runs exactly once.
sub via-reduce() is export {
    my $seen;
    reduce -> $acc, $j {
        my $a = BEGIN [];
        $a.push($j);
        $seen = $a;
        $acc + $j
    }, 0, |^4;
    $seen.elems
}

# Two textually identical BEGINs on one line are separate sites and keep
# separate values.
sub twice() is export {
    (BEGIN []) =:= (BEGIN [])
}
