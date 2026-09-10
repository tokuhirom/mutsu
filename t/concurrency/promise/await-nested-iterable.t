use Test;

# `await` must suspend for EVERY Promise it is given, no matter how deeply that
# Promise is nested inside list literals. The old implementation only descended
# two list levels, so a Promise nested deeper was pushed through as a raw value
# and never `.wait()`ed — `await` returned before its side effect landed
# (S17-promise/nonblocking-await.t test 28, PLAN.md 8.19). These tests force a
# deeply-nested slow Promise: if `await` skipped it, the atomic counter would be
# short by one when read immediately after `await` returns.

plan 4;

{
    # slow Promise at depth 3 (two inner lists deep); fast one at depth 1.
    my atomicint $x;
    sub slow1 { start { sleep 1; $x⚛++ } }
    sub fast1 { start { $x⚛++ } }
    await ( ( (slow1(),), ), fast1() );
    is $x, 2, 'await waits for a Promise nested three list-levels deep';
}

{
    # The exact shape from the roast test: nine Promises across several nesting
    # depths, awaited in sink context.
    my atomicint $x;
    sub p { start { sleep .1; $x⚛++ } }
    await (((p(), (p(), (p(),))), (p(), p(), p())), (p(), p(), p()));
    is $x, 9, 'await in sink context waits for all deeply-nested Promises';
}

{
    # Mixed depths; result list collects every leaf result in order.
    my @r = await ( (start { 1 },), (start { 2 }, (start { 3 },)) );
    is @r.sort, [1, 2, 3], 'nested await collects every result';
}

{
    # A flat list of Promises still works (regression guard).
    my @r = await (start { 10 }, start { 20 });
    is-deeply @r, [10, 20], 'flat list of promises still awaited';
}
