# A `supply` block's own lexicals alias the caller's same-named lexicals

A `my` variable declared inside `supply { ... }` is not private to the block. It
aliases a same-named lexical in the calling scope, in **both** directions:

## Direction 1 — the caller's value wins inside the block

```raku
sub mk($in) {
    supply {
        my $buffer = Buf.new;                      # empty
        whenever $in -> $packet {
            note "inner sees " ~ $buffer.raku;
            emit $buffer.elems;
        }
    }
}

my $buffer = Buf.new(1, 2, 3);                     # same name, caller scope
my $s = Supplier.new;
my $done = Promise.new;
my @g;
mk($s.Supply).schedule-on($*SCHEDULER).tap(-> $v { @g.push($v) }, done => { $done.keep });
start { $s.emit(Buf.new(9)); $s.done }
await Promise.anyof($done, Promise.in(3));
say @g.join(',');
```

raku prints `inner sees Buf.new()` and `0`. mutsu prints
`inner sees Buf.new(1,2,3)` and `3` — the block's own `$buffer` was replaced by
the caller's before the `whenever` body ran. Renaming either variable makes it
pass, which is the tell.

**The trigger is emitting from another thread.** With the same supply, tapping
and emitting on the main thread does not show it; wrapping the emit in
`start { ... }` does, with or without `.schedule-on($*SCHEDULER)`:

| shape | mutsu | raku |
| --- | --- | --- |
| tap + emit inline | (emits nothing at all — see below) | `INNER` |
| `.schedule-on` + emit inline | (emits nothing at all) | `INNER` |
| emit inside `start` | `OUTER` | `INNER` |
| `.schedule-on` + emit inside `start` | `OUTER` | `INNER` |

So the `whenever` body's free variables are being resolved against the
*emitting* thread's ambient env rather than the closure's captured env. The
emitting thread runs a clone of the interpreter whose env is the main script's,
so a name the closure did not actually capture falls through to the caller's
binding.

## Direction 2 — the block's value escapes to the caller

Independent of threads, and visible with an ordinary tap:

```raku
sub mk() { supply { my $buffer = "INNER"; $buffer = $buffer ~ "+"; emit $buffer } }
my $buffer = "OUTER";
mk().tap(-> $v { });
say $buffer;      # raku: OUTER    mutsu: INNER+
```

This half is the exit writeback in `call_sub_value`
(`src/runtime/resolution_call_sub.rs`, the `merged.insert_sym` loops). Both
branches already skip a value equal to the body-entry snapshot, but a name the
body *declared* with `my` is never equal to it — the snapshot holds the
**caller's** value, so a fresh `my` always looks like a mutation.

An attempted fix — skipping names in `cc.my_declared_sym` that are not also in
`cc.free_var_syms`, the same rule `push_block_declared_keys`
(`resolution_map_grep.rs`) and the compiled-closure exit merge
(`vm_closure_dispatch.rs`) already apply — does fix direction 2 and passes
`make test`, but it makes direction 1 *worse*: the `whenever` body then reads
the caller's value in cases that previously worked, because it had been relying
on the leak to see the supply body's lexical at all. The two directions are one
bug and must be fixed together.

## Why it matters

`Cro::HTTP2::FrameParser.transformer` is exactly the first shape:

```raku
supply {
    my $buffer = Buf.new;
    ...
    whenever $in -> Cro::TCP::Message $packet {
        my $data = $buffer ~ $packet.data;
        ...
    }
}
```

and `t/http2-frame-parser.rakutest` emits its packets from a `start` block while
holding its own `my $buffer` for the frame bytes. The parser therefore sees the
test's buffer prepended to the very first packet and rejects the HTTP/2 preface
with `X::Cro::HTTP2::IncorrectPreface`. That is all 16 of that file's failures,
and the same shape (`my $buffer` + `whenever`) appears in
`Cro::HTTP::RequestParser` and `Cro::HTTP::ResponseParser`.

## What a fix needs

The supply body must run in a scope of its own whose bindings the `whenever`
closures created inside it genuinely capture — not a flattened env shared with
the caller. Concretely:

- the body's `my` declarations must not be written back to the caller on exit
  (direction 2), and
- a `whenever` body closure must carry its own binding for those names so that
  invoking it from a thread clone, where the ambient env is the main script's,
  still resolves them (direction 1).

The second half is the `locals`/`env` dual store showing through: the name is
in the creating frame's locals, the closure captured no entry for it, and the
lookup falls through to whatever ambient env the invoking thread has. It is the
same family as
[`todo/deep/closure-env-capture-cost.md`](closure-env-capture-cost.md) and the
Slice F reverse-sync work; do not fix it with another writeback special case.

## Side finding

In the table above, tapping a supply returned from a sub and emitting on the
same thread produced **no values at all** in mutsu (`got=`), while raku emits
normally. That looks like a separate ordering bug (the tap registers after the
on-demand body has already been drained) and deserves its own reduction —
`tmp/supplylex7.p6` in the session scratch is the harness.
