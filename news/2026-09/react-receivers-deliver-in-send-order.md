# A react delivers its channel-receiver events in send order

[#9611](https://github.com/tokuhirom/mutsu/issues/9611): under load,

```raku
react {
    whenever $p.stdout { $out ~= $_ }
    whenever $p.start { done }
}
```

occasionally lost the last stdout chunk (6 losses in 160 iterations under GC stress on a
debug build). `Proc::Async`'s `proc-wait` thread joins both reader threads before it keeps
the `.start` Promise, so the final chunk is always queued first. But the react drive loop
polled its receiver-backed subscriptions once per round in index order: if the stdout
receiver was polled while still empty, and the chunk and the Promise's keep both landed
before the Promise's receiver was polled in the same round, `done` ran and the chunk was
dropped.

Every `SupplySender::send` now stamps its event with a process-wide sequence number,
assigned immediately before the send, so if one send happens-before another its sequence
is smaller. Before the drive loop delivers an event from one receiver, it first delivers
any event on the other receivers whose sequence is smaller, oldest first
(`deliver_receiver_poll_ordered` in the new `src/vm/vm_react_receiver.rs`). This is a
merge in true send order rather than "drain everything before a Promise": it never
reorders causally later events ahead, and a busy producer cannot starve a
`whenever Promise.in(1) { done }`, because everything it sends after the Promise resolves
has a larger sequence. `SupplyReceiver::peek_seq` waits for a concurrent pumper, so every
event sent before the peek is accounted for.

The same loop had 0 losses in 320 iterations after the change, and
`t/concurrency/react-receiver-send-order.t` pins it.
