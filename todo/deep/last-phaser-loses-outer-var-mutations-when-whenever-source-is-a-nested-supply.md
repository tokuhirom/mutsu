# A `whenever`'s `LAST` phaser reads a stale value of a supply-block-local `my` var when the source supply is itself derived from another `supply { whenever ... { emit } }`

## Symptom

```raku
my $source = Supplier.new;
my $inner = supply {
    whenever $source.Supply -> $x {
        emit $x;
    }
}
my $p = Promise(supply {
    my $joined = 0;
    whenever $inner -> $x {
        $joined += $x;
        LAST emit $joined;
    }
});
start {
    $source.emit(1);
    $source.emit(2);
    $source.done;
}
await Promise.anyof($p, Promise.in(3));
say "result: {$p.result}";
```

raku: `result: 3` (the `LAST` phaser sees the accumulated `$joined`).
mutsu: `result: 0` — the `LAST` phaser sees `$joined`'s INITIAL value, as if
none of the `$joined += $x` mutations inside the same `whenever` body were
visible to it.

## What isolates the trigger

- Replacing `whenever $inner -> $x {...}` with `whenever $source.Supply ->
  $x {...}` (removing the intermediate `$inner = supply { whenever { emit }
  }` layer, reading directly from the raw `Supplier`'s `.Supply`) makes it
  work correctly (`result: 3`). **The bug requires the `whenever`'s source
  to be a DERIVED supply — one itself built from another `supply { whenever
  ... { emit } }` — not a raw `Supplier.Supply`.**
- The shorthand phaser form (`LAST emit $joined;`) and the explicit block
  form (`LAST { emit $joined; }`) both reproduce identically — not specific
  to phaser syntax.
- `$joined` is declared as the FIRST statement inside the outer `supply {
  }` block (`my $joined = 0;`), not captured from an enclosing scope — the
  mutation (`$joined += $x`) and the read (`LAST emit $joined`) are
  textually in the SAME `whenever` body, so this is not an ordinary
  cross-closure capture problem; something about the `LAST` phaser
  specifically (or how it's compiled/dispatched) ends up disconnected from
  the live value even within the same block.
- A separate, cruder repro without `LAST` (mutating `$joined` inside a
  nested-supply `whenever` and reading it from OUTSIDE the whole reactive
  block afterward) does NOT reproduce a divergence — `raku` also shows the
  stale/zero value there, because that shape is genuinely unspecified
  (`start`/`react` concurrency ordering) in real Raku. Don't be misled by
  that shape; it is not evidence against this bug. The confirmed divergence
  is specifically the `LAST`-phaser-reads-a-value-it-should-see-within-the-
  same-callback case above.

## Why this matters (real-world impact)

This is the root cause of two Cro::HTTP::ResponseParser roast/vendored-suite
failures found in the 2026-08-12 Cro session
(`http-response-parser.rakutest` tests "Response with body terminated by
close of connection" and "Connection close with incomplete body throws").
`Cro::MessageWithBody.body-blob` (`vendor: Cro::Core`'s
`lib/Cro/MessageWithBody.rakumod`) is:

```raku
method body-blob(--> Promise) {
    Promise(supply {
        my $joined = Buf.new;
        whenever self.body-byte-stream -> $blob {
            $joined.append($blob);
            LAST emit $joined;
        }
    })
}
```

and for a response with no `content-length`/chunked encoding,
`self.body-byte-stream` resolves to `Cro::HTTP::RawBodyParser::UntilClosed`'s
`supply { whenever $raw-blobs { .emit; } }` — exactly the two-level nested
supply shape that triggers this bug. So `.body-blob`/`.body-text` on ANY
Cro message using the `UntilClosed` body parser resolves to the WRONG
(empty/stale) value instead of the accumulated body, or — in the full Cro
integration test — appears to hang entirely (the isolated repro above
resolves the Promise with the wrong value rather than hanging; the fuller
Cro shape may add enough extra supply/Promise-chaining layers, e.g.
`body-text`'s `.then`, that the difference manifests as a hang instead —
not yet confirmed which).

## Suggested next steps (not investigated further)

1. `rust-gdb` into the `LAST` phaser dispatch for a `whenever` (search
   `OpCode` variants around phaser/whenever handling, likely in
   `vm_scope_ops.rs`/`vm_control_ops.rs`/`resolution_call_sub.rs` per the
   ADR-0027 session's map of supply/whenever internals) and compare the env
   the `LAST` phaser's body executes under against the env the main
   `whenever` iteration body executes under — the fact that a raw-Supplier
   source works but a derived-supply source doesn't suggests the derived
   supply's OWN internal `whenever` (the `$inner`/`UntilClosed` one)
   introduces an extra frame/env boundary that the outer `LAST` phaser's
   lookup doesn't traverse correctly, while the ordinary per-iteration body
   does.
2. Check `code.inherited_owned_lexicals` / `pending_whenever_inherited_owned`
   (`src/runtime/resolution_call_sub.rs`, `src/runtime/resolution_eval.rs`,
   `src/vm/vm_scope_ops.rs::exec_whenever_scope_op`) — the ADR-0027 Slice 2
   audit (2026-08-12) confirmed these are unrelated to the LOOP-frozen-value
   cascade, but they are exactly the "which lexical does a nested-whenever's
   dispatch resolve a name to" machinery, and this bug's shape (nested
   whenever, name resolution divergence) is suspiciously adjacent — worth
   ruling in/out early.
3. ~~Try a same-shape repro with a NON-phaser read~~ — DONE, see below: the
   bug is NOT `LAST`-specific. It is a more general "`emit` from inside a
   `whenever` whose source is a nested/derived supply doesn't reliably reach
   a `Promise(supply {...})` coercion wrapping that `whenever`" issue.

### Update: not LAST-phaser-specific — a plain conditional `emit` inside the nested whenever also fails, differently

```raku
my $source = Supplier.new;
my $inner = supply {
    whenever $source.Supply -> $x { emit $x; }
}
my $p = Promise(supply {
    my $joined = 0;
    whenever $inner -> $x {
        $joined += $x;
        if $x == 2 { emit $joined; }   # ordinary emit, no LAST phaser at all
    }
});
start { $source.emit(1); $source.emit(2); $source.done; }
await Promise.anyof($p, Promise.in(3));
say "result: {$p.result}";
```

raku: `result: 3`. mutsu: `Use of Nil in string context` / `result: ` (the
`Promise` completes but with no captured value at all — worse than the
`LAST` case, which at least completed with a *wrong* value of `0`). This
narrows the bug to: **`Promise(supply { ... whenever <nested-supply> -> $x
{ ... emit ... } ... })` does not reliably capture an `emit`ted value when
the whenever's source is itself a derived supply**, independent of whether
the emit happens via `LAST` or an ordinary statement. The `LAST` case's
"wrong value of 0" vs this case's "no value at all" are likely two visible
symptoms of the same underlying mechanism (e.g. the coercion's internal
supplier/promise-resolution hookup racing against or missing the nested
whenever's completion signal), not two separate bugs — but that is not
confirmed.

## Reproduce

The repro at the top of this file, no fixtures needed. Expected (raku):
`result: 3`. Actual (mutsu): `result: 0`.
