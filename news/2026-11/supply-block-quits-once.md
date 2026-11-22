# A supply block quits once

A `supply { }` block now delivers its `quit` exactly once. When one of its
`whenever` sources quit it, a later failure of another `whenever` -- a source
whose tap failed (`Supply.interval` on a `CurrentThreadScheduler`), a source
that quit later, or the body dying -- reached the tap's `quit` handler again.
Since the live `map` stages became per-tap recipes (ADR-18443), a dying
`map` under a `whenever` quits through that path, which made EventSource::Server's
`t/040-stop.t` keep a Promise twice and die with "already vowed".

The block's emitter now carries a terminated flag that survives the reset
`Supplier.quit` does after delivering, and every quit route of a block
(`call_block_quit_handler`, the `whenever` quit forwarder) checks and sets it.
