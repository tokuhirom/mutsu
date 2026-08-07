# A bare `emit` inside a supply block's nested `sub` leaks into the outer supply

The parser rewrites an `emit` written **directly** in a `supply { }` body to
`$emitter.emit(...)`. An `emit` written inside a `sub` *declared in* that body is
not rewritten, so at run time it falls through to `Interpreter::call_function`'s
`emit` arm (`src/runtime/builtins.rs:886`), which resolves the target
dynamically:

```rust
if let Some(emitter) = self.active_supply_emitters.last().cloned() {
    return self.call_method_with_values(emitter, "emit", vec![value]).map(|_| Value::NIL);
}
```

That is the right idea — Raku catches `emit` with the innermost *dynamically*
enclosing supply — but when an inner supply's `whenever` body runs **while an
outer supply's body is still on the stack**, `active_supply_emitters.last()` is
the *outer* emitter, and the inner supply's value is emitted downstream of the
wrong supply.

## Reproduction

```raku
sub v2(Supply $raw) {
    supply {
        whenever $raw -> $v { twice($v) }
        sub twice($x) { emit $x * 2 }      # <-- not rewritten to $emitter.emit
    }
}

my $wire = Supplier.new;
my $raw;
my @bodies;
my $outer = supply {
    whenever $wire -> $v {
        if !$raw.defined {
            $raw = Supplier.new;
            emit v2($raw.Supply);          # hand the inner supply downstream
        }
        $raw.emit($v);                     # runs the inner whenever *inside* this body
    }
};
$outer.tap(-> $b { @bodies.push($b) });
$wire.emit(1);
@bodies[0].tap(-> $x { }) if @bodies;
$wire.emit(2);

say @bodies.elems;                  # raku: 1     mutsu: 2
say @bodies.map(*.WHICH).raku;      # mutsu: (ObjAt "Supply|43", ValueObjAt "Int|4")
```

The second element of `@bodies` is the **Int 4** — `twice(2)`'s value, which
belongs to the inner supply, arriving at the *outer* supply's tap. The outer
lexicals are fine (`$raw` is still defined on the second call, verified by
recording it into an array), so this is purely mis-routed emission.

The trigger is exactly the nested `sub`. Four variants of the inner parser,
identical except for how the value is emitted:

| inner parser | mutsu |
| --- | --- |
| `whenever $raw -> $v { emit $v * 2 }` | correct |
| `my @buf; whenever $raw -> $v { @buf.push($v); emit @buf.shift * 2 }` | correct |
| `whenever $raw -> $v { twice($v) }` + `sub twice($x) { emit $x * 2 }` after | **leaks** |
| same, with the `sub` declared before the `whenever` | **leaks** |

An inner supply that is *not* created inside the outer supply's `whenever` body
is fine — the outer emitter has to be dynamically active at the moment the inner
`whenever` fires.

## Why it matters: Cro cannot read a chunked response body

`Cro::HTTP::RawBodyParser::Chunked.parser` is precisely this shape:

```raku
supply {
    my $state = AwaitingLength;
    my $buffer = Buf.new;
    whenever $raw_blobs -> $blob { $buffer.append($blob); parse-chunks(); }
    sub parse-chunks() { ... emit $buffer.subbuf(0, $length-awaited); ... }
}
```

and `Cro::HTTP::ResponseParser` creates and feeds it from **inside its own**
`whenever $in` body:

```raku
$raw-body-byte-stream = Supplier.new;
$response.set-body-byte-stream(preserve(
    $raw-body-parser.parser($response, $raw-body-byte-stream.Supply, $leftover)));
$raw-body-byte-stream.emit($header-decoder.consume-exactly-bytes($count));
```

So `parse-chunks`'s decoded `Buf` is emitted out of the **ResponseParser's**
supply instead of the body supply, and the client dies with

```
No such method 'data' for invocant of type 'Buf'
```

(`.data` is `Cro::TCP::Message.data`, read by the ResponseParser itself). The
`ContentLength` parser emits inline rather than from a nested sub, which is why
`Content-Length` bodies work and chunked ones do not.

`curl` reads a chunked response from a mutsu Cro **server** correctly, so only
the client is affected.

### Cro-free client reproduction

```raku
# tmp/chunked-srv.raku -- run under plain raku; uses no Cro
my $listen = IO::Socket::Async.listen('localhost', 31318);
my $tap = $listen.tap(-> $conn {
    my $body = "HTTP/1.1 200 OK\r\nTransfer-encoding: chunked\r\n\r\n1\r\n1\r\n0\r\n\r\n";
    $conn.Supply(:bin).tap(-> $ { });
    $conn.print($body);
    Promise.in(0.3).then({ $conn.close });
});
say "listening";
sleep 25;
```

```raku
# tmp/chunked-cli.raku
use Cro::HTTP::Client;
my $resp = await Cro::HTTP::Client.get('http://localhost:31318/x');
say "body = ", (await $resp.body-text).raku;   # raku: "1"   mutsu: dies
```

It is the remaining failure in Cro's `t/http-middleware.rakutest` subtest 4: the
cache middleware answers the second request with a body that carries no
`Content-Length`, so `Cro::HTTP::ResponseSerializer` frames it chunked, and the
in-process client then cannot read it.

## Where the fix belongs

`Interpreter::call_supply_tap` (`src/runtime/supply_promise.rs`) already exists
for exactly this purpose: it finds the `__mutsu_supply_emitter_<id>` lexical in
the *callback's own* env and pushes it onto `active_supply_emitters` for the
duration of the call, so a bare `emit` in a sub the callback calls resolves to
the right supply. The bug is that this does not hold for the failing shape —
either the whenever body no longer captures that lexical once the block also
declares a `sub` (the free-variable/`owned_lexicals` analysis changes), or the
body reaches dispatch through a path that does not push (note that
`Interpreter::call_react_callback` in `src/vm/vm_react_loop.rs` runs whenever
bodies via `vm_call_map_block` and does **not** push an emitter). A `gdb` break
on `supply_promise.rs`'s `let res = self.call_sub_value(tap, ...)` shows a mix of
`pushed = true` and `pushed = false` dispatches on the reproduction above;
identifying which one is the inner whenever body is the next concrete step.

The durable fix is to make the emitter dynamically scoped to *whichever* supply
a callback belongs to, at every dispatch site — not just the `call_supply_tap`
one — so that a nested supply's callbacks never inherit the enclosing supply's
emitter.

## Related

- `todo/deep/compiled-fns-default-breaks-nested-subs-outside-methods.md` — nested
  named subs and their captured environment.
- The misleading `Impossible coercion from 'Any' into 'Promise'` that hid this
  for a whole investigation is fixed in
  `news/2026-08/coercion-method-errors-no-longer-swallowed.md`.
