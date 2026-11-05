# A nested `whenever` over a cold `supply` that reads a `Channel.Supply` gets its values

In `react { whenever $a -> $x { whenever supply { whenever $s.Supply.Channel.Supply { emit $_ } } -> $v { ... } } }`
the inner `whenever` is registered while the drive loop runs, and the loop adopts
it through `value_to_react_subscription`. That builder had no arm for a
`Channel.Supply` (the one `build_react_subscriptions` has), so the supply body's
inner subscription was dropped and replayed as a finite source, and nothing fed
to the channel afterwards arrived. It now drains the channel like the top-level
builder. This is what made `IO::Socket::Async::SSL`'s server never see the
bytes a client wrote: its `Supply(:enc)` is exactly such a `supply { whenever
$!bytes-received.Supply.Channel.Supply { ... } }` (#16026).
