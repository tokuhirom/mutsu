use Test;

# ADR-0019 C6e-2: a `take` that fires inside a routine CALLED from a gather
# body (rather than in the body's own code) cannot suspend the lazy-pull
# coroutine soundly — the driver snapshots only its own frame, so the old
# suspension signal unwound the callee and corrupted the saved ip/stack
# ("Interpreter stack underflow in CallFunc" for a compiled callee, silently
# missing elements for an interpreter-arm one). `take_value` now keeps
# collecting eagerly when the take arrives from a nested call frame
# (`lazy_pull_entry_call_depth`), so these shapes produce every element.

plan 6;

sub trip($n) { for 1..2 -> \a { take a * $n } }
sub trip-sigilless(\N) { for 1..3 -> \a { take a * N } }

{
    my @a = gather trip(5);
    is-deeply @a, [5, 10], 'assignment materializes takes from a called sub';
}
{
    my @got;
    for gather trip(5) { @got.push($_) }
    is-deeply @got, [5, 10], 'statement for iterates takes from a called sub';
}
{
    my $s := gather trip(7);
    is "$s[0] $s[1]", '7 14', 'indexed access pulls takes from a called sub';
}
{
    my @a = gather trip-sigilless(10);
    is-deeply @a, [10, 20, 30], 'sigilless-param callee takes materialize';
}
{
    # The advent2012-day04.t problem-9 shape: nested gather, the inner one
    # over a sub call, consumed by a statement-modifier for.
    my @r = gather {
        sub triplets(\N) {
            for 1..3 -> \a {
                take $(a, a + N);
            }
        }
        take .list[1] for gather triplets(10);
    };
    is-deeply @r, [11, 12, 13], 'nested gather over a called sub';
}
{
    # A take at the gather body's own depth still suspends lazily: the
    # infinite gather below must terminate via the bounded slice.
    my $inf := gather { my $i = 0; loop { take ++$i } };
    is-deeply $inf[^3].list, (1, 2, 3), 'own-frame takes stay lazily suspendable';
}
