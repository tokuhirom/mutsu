use Test;

# `$p.value++` / `$p.value--` on a Pair whose value was captured from a
# container must accumulate across loop iterations, exactly as it does when the
# statements are written out one after another.
#
# The `++`/`--` lowering on an rw-accessor lvalue reads the accessor into a
# compiler-synthesized temp global, bumps that temp, and writes it back through
# the accessor. The accessor read hands back the target's own container here,
# so without a deref the temp global stayed BOUND to that cell: on the next
# iteration `SetGlobal` wrote the freshly-read cell *through* the binding,
# storing the container into itself. The increment stalled on iteration 2 and
# every later read of the now self-referential cell recursed until the stack
# overflowed.

plan 14;

{
    my $n = 0;
    my $r = (n => $n);
    my @log;
    for 1..3 { $r.value++; @log.push($r.value.Int) }
    is-deeply @log, [1, 2, 3], '.value++ accumulates across loop iterations';
    is $n, 3, 'and the increments reached the source container';
}

{
    my $n = 5;
    my $r = (n => $n);
    for 1..3 { $r.value-- }
    is $n, 2, '.value-- accumulates across loop iterations';
    is $r.value, 2, 'and the pair reads the same value back';
}

# The prefix forms go through their own lowering with the same temp.
{
    my $n = 0;
    my $r = (n => $n);
    my @log;
    for 1..3 { @log.push((++$r.value).Int) }
    is-deeply @log, [1, 2, 3], 'prefix ++ yields the new value each iteration';
    is $n, 3, 'and accumulates into the source container';
}
{
    my $n = 3;
    my $r = (n => $n);
    for 1..3 { --$r.value }
    is $n, 0, 'prefix -- accumulates too';
}

# A `while` loop reuses the same temp the same way.
{
    my $n = 0;
    my $r = (n => $n);
    my $i = 0;
    while $i < 4 { $r.value++; $i++ }
    is $n, 4, '.value++ accumulates in a while loop';
}

# Postfix still yields the OLD value, and prefix the NEW one.
{
    my $n = 7;
    my $r = (n => $n);
    is $r.value++, 7, 'postfix ++ returns the old value';
    is $r.value, 8, 'and the pair now holds the new one';
}

# The same lowering serves ordinary `is rw` accessors; they must not regress.
{
    class C { has $.c is rw }
    my $o = C.new(c => 0);
    for 1..3 { $o.c++ }
    is $o.c, 3, 'an is-rw attribute accessor still accumulates in a loop';
}

# ... and a mutable QuantHash weight, which takes the writeback arm instead of
# a container cell.
{
    my $bh = <a a b b b>.BagHash;
    for $bh.pairs { .value-- }
    is $bh<a>, 1, 'BagHash weight decrements in a loop (a)';
    is $bh<b>, 2, 'BagHash weight decrements in a loop (b)';
}

# A Pair over a literal has no container at all: assignment is refused rather
# than faked, so the increment dies instead of silently stalling.
{
    my $p = (n => 0);
    dies-ok { for 1..3 { $p.value++ } }, 'a literal Pair value cannot be incremented';
}
