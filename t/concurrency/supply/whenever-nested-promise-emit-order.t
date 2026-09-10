use Test;

# A `whenever <Promise>` nested inside a supply block's `whenever` body emits
# into the enclosing block, so its emissions must land in the block's emission
# order (#7824).
#
# They did not: `$p.keep` handed the promise's waiters to `worker_pool::submit`,
# one independent pooled task per resolved promise. Order was preserved only
# *within* one promise's waiter list; across promises the pool workers raced, so
# 1,2,3 came out as 1,3,2. Idle machines hid it (0 failures in 5 runs); under
# `scripts/flake-repro.sh -l 4` it was 6 failures in 20, and it failed the CI
# `test` job (which runs `prove -j4`) twice in a row.
#
# Such a body now runs inline on the resolving thread, inside the enclosing
# block's serialize group, which is re-entrant on the same thread.

plan 3;

# The core ordering guarantee, run several times: one bad interleaving in any
# round fails the test, which is what makes this a useful pin under load.
my @rounds;
for 1..5 {
    my $sup = Supplier.new;
    my @fired;
    my $done = Promise.new;
    supply {
        whenever $sup -> $v {
            my $p = Promise.new;
            whenever $p { emit $v * 10 }
            $p.keep;
        }
    }.tap({ @fired.push($_); $done.keep if @fired.elems == 3 });
    $sup.emit($_) for 1..3;
    await Promise.anyof($done, Promise.in(10));
    @rounds.push(@fired.join(','));
}
is @rounds.unique, ['10,20,30'],
    'a nested whenever-on-Promise emits in the enclosing block emission order';

# More values, to catch an ordering scheme that only holds for three.
my $sup2 = Supplier.new;
my @many;
my $done2 = Promise.new;
supply {
    whenever $sup2 -> $v {
        my $p = Promise.new;
        whenever $p { emit $v }
        $p.keep;
    }
}.tap({ @many.push($_); $done2.keep if @many.elems == 12 });
$sup2.emit($_) for 1..12;
await Promise.anyof($done2, Promise.in(10));
is @many, [1..12], 'the order holds across a longer run of emissions';

# A `whenever` on a promise OUTSIDE any supply block has no enclosing order to
# preserve and stays on the pooled path; it must still fire.
my $solo = Promise.new;
my $seen = Promise.new;
react {
    whenever $solo -> $v { $seen.keep($v); done }
    $solo.keep(99);
}
is await(Promise.anyof($seen, Promise.in(10))) && $seen.result, 99,
    'a whenever on a bare promise still fires';
