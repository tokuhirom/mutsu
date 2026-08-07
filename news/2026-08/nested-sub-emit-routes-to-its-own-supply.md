# A nested sub's bare `emit`/`done` now reaches its own supply

`emit` and `done` written **directly** in a `supply { … }` body are rewritten by
the parser to `$__mutsu_supply_emitter_N.emit(…)` / `.done()`. Written inside a
`sub` *declared in* that body they are not, so they fall through to the runtime's
dynamic resolution — `active_supply_emitters.last()` for `emit`, and a bare
react-done control signal for `done`. Both resolved to the wrong supply whenever
an inner supply's `whenever` fired while an outer supply's body was still on the
stack, which is exactly how Cro drives a chunked response body:

```raku
supply {                                  # Cro::HTTP::RawBodyParser::Chunked
    my $buffer = Buf.new;
    whenever $raw-blobs -> $blob { $buffer.append($blob); parse-chunks() }
    sub parse-chunks() { … emit $buffer.subbuf(0, $length-awaited); … done }
}
```

created and fed from inside `Cro::HTTP::ResponseParser`'s own `whenever $in`
body. The decoded `Buf` was emitted out of the *ResponseParser's* supply, so a
Cro client reading a chunked response died with `No such method 'data' for
invocant of type 'Buf'`, and `parse-chunks`'s `done` tore down the ResponseParser
instead of the body stream. `Content-Length` bodies were unaffected because that
parser emits inline.

## The emitter was picked by `HashMap` iteration order

`call_supply_tap` already existed to make a callback's own emitter the innermost
dynamically active one; it found it by scanning the callback's captured env for a
key starting with `__mutsu_supply_emitter_`. But a callback captures the *whole*
live env, so an inner supply's `whenever` — registered while the outer supply's
body runs — captures **both** blocks' emitter bindings, and `keys().find(…)`
picked whichever the hash happened to yield first. The same program could route
correctly or leak depending on nothing but the hash seed, which is why the bug
looked intermittent and why a reduced reproduction kept "fixing itself".

The callbacks now carry the answer explicitly. `run_whenever_with_value` stamps
every `whenever`/`LAST`/`QUIT` callback with `__mutsu_whenever_emitter`, taken
from `active_supply_emitters.last()` at callback-creation time — the moment when
the enclosing `supply` block is unambiguously the one whose body is running.
`call_supply_tap` prefers that stamp and only falls back to the prefix scan for
callbacks that carry no stamp (a plain `.tap` block written inside a supply).

## `done` unwinds to its own supply block

Rakudo's `done` unwinds to the enclosing `supply` block and completes that
supply. In mutsu an unrewritten `done` raised a react-done signal that travelled
straight through `Supplier.emit` into whoever emitted the value — an outer
supply's `whenever` body — terminating it instead. `call_supply_tap` now consumes
that signal for a stamped callback: it calls `done` on the callback's own emitter
and returns normally. A react block's `done` is untouched, since react callbacks
carry no stamp.

## Result

Cro's `t/http-middleware.rakutest` goes from 22 to 23 of 24 subtests, with the
`Cro::HTTP::Middleware::RequestResponse` subtest (a caching middleware whose
replayed response is framed chunked) now passing end to end. A Cro client reading
a chunked response from a plain `IO::Socket::Async` server returns its body
instead of dying.

Pinned by `t/supply-nested-sub-emit-routes-to-own-supply.t`.
