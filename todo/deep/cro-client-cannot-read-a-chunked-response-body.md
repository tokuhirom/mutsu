# Cro's HTTP client cannot read a chunked response body

`Cro::HTTP::Client` under mutsu reads a `Content-Length`-framed response body
fine but dies on a `Transfer-Encoding: chunked` one:

```
Died because of the exception:
    No such method 'data' for invocant of type 'Buf'
  in sub body-blob ...
  in sub body-text ...
```

**Root-caused**: see
`todo/deep/nested-sub-emit-leaks-into-the-outer-supply.md`. A bare `emit` inside
a `sub` declared in a `supply { }` body is not rewritten to `$emitter.emit(...)`
and resolves dynamically to `active_supply_emitters.last()`, which is the
*outer* supply when the inner supply's `whenever` fires inside the outer body.
`Cro::HTTP::RawBodyParser::Chunked` emits from exactly such a nested
`sub parse-chunks()`, and `Cro::HTTP::ResponseParser` creates and feeds it from
inside its own `whenever $in` body — so the decoded `Buf` is emitted out of the
ResponseParser's supply instead of the body supply. `.data` is
`Cro::TCP::Message.data`, read by the ResponseParser itself. The
`ContentLength` parser emits inline rather than from a nested sub, which is why
`Content-Length` bodies work and chunked ones do not. (The `Cro::TCP::Message`
type constraint on the parameter is also not enforced, which is why it surfaces
as a missing method rather than a binding failure.)

The mutsu **server** side is fine: `curl` reads a chunked response from a mutsu
Cro server correctly, body and all.

## Why it matters

It is the remaining failure in Cro's `t/http-middleware.rakutest` subtest 4
(`Cro::HTTP::Middleware::RequestResponse`). That test's cache middleware answers
the second request with

```raku
given Cro::HTTP::Response.new(:$request, :200status) {
    .set-body-byte-stream: supply emit $!cached-blob;
    .emit;
}
```

which carries no `Content-Length`, so `Cro::HTTP::ResponseSerializer` frames it
chunked — and the in-process client then cannot read it.

## Reproduction (no Cro server needed)

`tmp/` is scratch, so recreate these two files:

```raku
# tmp/chunked-srv.raku -- a raw socket server that answers one chunked response
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
say "status = ", $resp.status;
say "body   = ", (await $resp.body-text).raku;
```

Run the server under plain `raku` (it uses no Cro), then the client under each:
real `raku` prints `body = "1"`; mutsu dies as above. `curl` against the same
server is also fine, so the response itself is well-formed.

## Where to look

`Cro::HTTP::RawBodyParser::Chunked.parser` (in the Cro::HTTP dist's
`lib/Cro/HTTP/RawBodyParser.rakumod`) is a `supply { }` whose `whenever
$raw-blobs` body calls a `sub parse-chunks()` declared *after* it in the same
block. `Cro::HTTP::ResponseParser` wires it up as

```raku
$raw-body-byte-stream = Supplier.new;
$response.set-body-byte-stream(preserve(
    $raw-body-parser.parser($response, $raw-body-byte-stream.Supply, $leftover)));
$raw-body-byte-stream.emit($header-decoder.consume-exactly-bytes($count));
```

all from *inside* its own `whenever $in` body — a supply created and fed from
within another supply's callback, with a `preserve` in between.

The `ContentLength` parser has the same outer shape and works; the difference is
the nested `sub` (see the root-cause ticket above).

## Narrowing

The step-by-step narrowing from this failure down to the nested-`sub` emit leak
— including the four-variant table that isolates the trigger — lives in
`todo/deep/nested-sub-emit-leaks-into-the-outer-supply.md`.

## Note

Diagnosing this was much harder than it should have been because
`Promise($supply)` swallowed the real error and reported
`Impossible coercion from 'Any' into 'Promise'`. That is fixed separately
(`news/2026-08/coercion-method-errors-no-longer-swallowed.md`).
