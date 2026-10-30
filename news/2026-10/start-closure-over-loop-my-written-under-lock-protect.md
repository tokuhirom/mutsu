# A closure a spawned block calls no longer resolves its scalars to another iteration's binding

```raku
for 1..40 -> $i {
    my $promise = Promise.new; my $vow = $promise.vow; my $lock = Lock.new; my $done = False;
    my $work = { my $mine = False; $lock.protect: { $mine = !$done; $done = True }; $vow.keep($i) if $mine };
    start { $work() };
}
```

About a quarter of these promises stayed `Planned` forever (`raku`: all kept).
`start` keeps the scalars the block captures *itself* off the process-wide,
name-keyed cross-thread store (`block_captured_scalars`), where each loop
iteration's `my $done` would otherwise collide with the others'. The block here
only calls `$work`, so `$done` is a free variable of `$work`'s code and not of
the block's: it stayed on the name lane and a worker read the `$done` another
iteration's worker had already set. The captured scalars now include the free
variables of the closures the block calls through a variable it captures.

`MCP::Client`'s `t/13-llm-bridge.rakutest` hung on exactly this shape
(`InProcessTransport.request`'s `settle`/`$done` claimed by both the handler and
the `Promise.in` timeout); it now finishes in 1.4 s with 55 ok, the same as
`raku`.
