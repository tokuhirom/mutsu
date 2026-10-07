use Test;

plan 1;

sub bump-rw($p is rw) { $p⚛++ }

# A name-keyed atomic use in an earlier lexical scope must not make the later
# same-named atomic lexical's captured rw argument resolve to the old binding.
# `worker` has no direct assignment to `$n`; passing it to `bump-rw` is still a
# possible write and requires the declaring frame to share one cell with every
# worker.
{
    my atomicint $n = 0;
    $n⚛++;
}

{
    my atomicint $n = 0;
    sub worker { bump-rw($n) }
    await (^3).map: { start { worker() } };
    is $n, 3, 'same-named later atomicint stays shared through named-sub rw forwarding';
}
