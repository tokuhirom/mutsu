# `Supply.lines` silently drops every value from a channel-backed Supply

`.lines` works on a Supply whose values arrive through the *supplier* registry
(`Supplier.new.Supply`), but emits nothing at all for a Supply whose values
arrive through a **supply channel** — the shape used by real TCP sockets and by
the listener's accept stream.

```raku
# server side, run with a client sending "hello\n"
react {
    whenever IO::Socket::Async.listen('127.0.0.1', 31441) -> $conn {
        whenever $conn.Supply       -> $t { say "text: $t" }   # fires
        whenever $conn.Supply(:bin) -> $b { say "bin: $b" }    # fires
        whenever $conn.Supply.lines -> $l { say "line: $l" }   # never fires
    }
}
```

The client side is affected identically, which is why
`t/io-socket-async-real-connect.t` uses the plain text `.Supply` rather than the
more natural `.lines`.

## Root cause

`native_supply_dispatch.rs`'s `"lines"` arm builds the derived Supply with a
**fresh** `supply_id` (`next_supply_id()`) and `live => False`, carrying the
source's id only as an inert `parent_supply_id` attribute. For a supplier-backed
source it also copies `supplier_id`, which is what makes that case work — the
derived supply is still registered against the same supplier.

A real-TCP `.Supply` has no `supplier_id`. Its values are pushed down a channel
registered in `supply_channel_map()` under the source's `supply_id`, and the tap
path drains that channel by looking up `supply_id`. The derived lines Supply's
new id has no channel, so the tap has nothing to drain and the parent's channel
is never taken.

## Fix sketch

The derived supply must stay attached to the source's channel, and the line
splitting has to happen as chunks are drained rather than up front:

- keep the source's `supply_id` on the derived Supply (or teach the drain path
  to follow `parent_supply_id`), and
- have the drain path honour the existing `is_lines` / `line_chomp` attributes,
  buffering a partial trailing line between chunks the same way
  `split_supply_chunks_into_lines` does for a static value list.

The buffering detail matters: a TCP read boundary can land mid-line, so a
per-chunk split without carry-over would emit truncated lines.

## Repro

`tmp/srv2.log` / `tmp/srv4.log` in the session that filed this (recreate from the
snippet above — `tmp/` is gitignored).
