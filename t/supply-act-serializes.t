use Test;

# `Supply.act` differs from `Supply.tap` by exactly one guarantee: "the given
# code is guaranteed to be executed by only one thread at a time"
# (raku-doc Type/Supply.rakudoc). mutsu delivers a tap synchronously on
# whichever thread emitted, so concurrent `start` blocks emitting into one
# Supplier used to run the act callbacks at the same time -- letting two
# threads mutate one captured container simultaneously, which lost updates and
# (todo/deep/procasync-stress-segv.md) corrupted the heap.

plan 3;

# 1. One act tap, many concurrent emitters: the body never overlaps itself.
{
    my $supplier = Supplier.new;
    my $supply = $supplier.Supply;
    my $inside = 0;
    my $max = 0;
    $supply.act: {
        my $n = ++$inside;
        $max = $n if $n > $max;
        sleep 0.02;
        $inside--;
    }
    await do for ^8 { start { $supplier.emit($_) } }
    is $max, 1, 'an act callback never runs concurrently with itself';
}

# 2. Sibling act taps on the same Supply do not overlap either. This is
#    STRONGER than the documented guarantee and stronger than rakudo 2026.06,
#    which serializes each act callback only against itself (this same file
#    reports max=2 under rakudo). mutsu runs all of a supplier's taps in one
#    dispatch loop, so the dispatch is the unit it can lock -- and locking it is
#    what makes a container shared by several act bodies safe against concurrent
#    emitters, which is the whole point (the roast integration test this came
#    from writes "assume .act serializes" over exactly that shape). A strictly
#    stronger mutual exclusion can only reduce concurrency, never change a
#    correct program's result, so pinning it here is safe.
{
    my $supplier = Supplier.new;
    my $supply = $supplier.Supply;
    my $inside = 0;
    my $max = 0;
    for ^3 {
        $supply.act: {
            my $n = ++$inside;
            $max = $n if $n > $max;
            sleep 0.01;
            $inside--;
        }
    }
    await do for ^6 { start { $supplier.emit($_) } }
    is $max, 1, 'sibling act taps on one Supply do not overlap (stronger than rakudo)';
}

# 3. The consequence the crash ticket cares about: every act write to a shared
#    array survives. Each emitted n touches only index n, so the result is
#    fully determined no matter how the emits interleave -- any missing slot is
#    a lost update from an unsynchronized concurrent write.
{
    my $supplier = Supplier.new;
    my $supply = $supplier.Supply;
    my @seen;
    $supply.act: { @seen[$_]   = "Fizz" if $_ %% 3 }
    $supply.act: { @seen[$_]  ~= "Buzz" if $_ %% 5 }
    $supply.act: { @seen[$_] //= $_ }
    await do for 1..20 { start { $supplier.emit($_) } }
    is @seen[1..20].join(' '),
        '1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz',
        'concurrent act writes to one shared array all land';
}

# vim: expandtab shiftwidth=4
