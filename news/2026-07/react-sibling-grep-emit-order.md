# Sibling `whenever $s.grep(...)` supplies now replay buffered values in emit order

Two sibling `whenever $s.grep(...)` in one `react` must deliver in global emit
order:

```raku
my @out;
my $inputs = Supplier::Preserving.new;
my $s = $inputs.Supply;
react {
    whenever $s.grep(* > 0) { @out.push: "p$_" }
    whenever $s.grep(* < 0) { @out.push: "n$_" }
    whenever start {
        for 1..3 { $inputs.emit($_); $inputs.emit(-$_) }
    } { done }
}
# expected: p1,n-1,p2,n-2,p3,n-3
```

Under load mutsu intermittently produced the per-supply-batched
`p1,p2,p3,n-1,n-2,n-3` instead (`t/supply-live-grep-map-react-order.t` test 5).

## Root cause

Each `$s.grep(...)` builds a distinct live *derived* supplier that buffers its
filtered values in the process-global supplier registry. The react drive loop
registers a push sink on each derived supplier and drains one shared,
emit-ordered waker queue — but the sinks were registered one at a time, and
`supplier_sink_register` replays *one* supplier's whole buffer before the next
subscribes. That is only observable when the `whenever start { emit … }`
producer thread races ahead of the drive loop's sink registration and buffers
values before the sinks exist: at registration the positive supplier already
holds `[1, 2, 3]` and the negative `[-1, -2, -3]`, so replaying them
supplier-by-supplier yields `p1,p2,p3,n-1,n-2,n-3`, losing the interleave. A
20 ms delay injected before sink registration reproduced it 5/5.

## Fix

Every `supplier_emit`/`supplier_done`/`supplier_quit` now stamps the event with
a process-global monotonic sequence number (parallel `emitted_seq` vector plus a
`terminal_seq`). A new `supplier_sinks_register_batch` registers all of a
react's supplier sinks under a single registry-lock acquisition, collects each
supplier's buffered events with their sequence stamps, and replays the combined
set **sorted by sequence** — so sibling derived supplies interleave in true emit
order. Future live emits (pushed after registration) are naturally later. The
single-sink callers keep using `supplier_sink_register` unchanged.

With the fix the forced-race reproduction is `p1,n-1,p2,n-2,p3,n-3` 5/5. Pinned
by `t/supply-live-grep-map-react-order.t` test 5.
