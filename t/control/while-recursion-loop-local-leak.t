use v6;
use Test;

# A routine that recurses from inside its own `while` loop body must not have the
# callee's body-local `my $x` register in the CALLER's active loop-local
# declaration scope. Previously the positional/typed light-call fast paths (which
# bypass `push_call_frame`) failed to isolate `loop_local_vars`/
# `loop_local_saved_env`, so the callee's `my $r` polluted the caller's while
# loop, and the caller's loop-exit restore reverted its own `$r`.

plan 5;

# Accumulator declared before the loop, mutated after the recursive call.
sub w($n) {
    my $r = 0;
    my @work = (1,);
    while @work.splice {
        w($n - 1) if $n > 0;
        $r += 10;
    }
    $r
}
is w(1), 10, 'while-body recursion does not clobber a scalar accumulator (depth 1)';
is w(3), 10, 'while-body recursion does not clobber a scalar accumulator (depth 3)';

# Explicit return path.
sub v($n) {
    my $r = 0;
    my @work = (1,);
    while @work.splice {
        v($n - 1) if $n > 0;
        $r += 5;
    }
    return $r;
}
is v(2), 5, 'explicit return after while-body recursion keeps the accumulator';

# Array accumulator (was already correct; guard against regression).
sub a($n) {
    my @acc;
    my @work = (1, 2);
    while @work.splice(0, 1) -> @batch {
        a($n - 1) if $n > 0;
        @acc.push(@batch[0]);
    }
    @acc
}
is a(1).elems, 2, 'array accumulator across while-body recursion is intact';

# Mutual recursion with differently-named accumulators.
sub p($n) { my $rp = 0; my @w = (1,); while @w.splice { q($n-1) if $n > 0; $rp += 10 }; $rp }
sub q($n) { my $rq = 0; my @w = (1,); while @w.splice { p($n-1) if $n > 0; $rq += 10 }; $rq }
is p(2), 10, 'mutual recursion through while loops keeps each accumulator';
