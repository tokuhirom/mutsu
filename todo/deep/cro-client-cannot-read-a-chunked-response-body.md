# Cro's HTTP client cannot read a chunked response body

`Cro::HTTP::Client` under mutsu reads a `Content-Length`-framed response body
fine but dies on a `Transfer-Encoding: chunked` one:

```
Died because of the exception:
    No such method 'data' for invocant of type 'Buf'
  in sub body-blob ...
  in sub body-text ...
```

`.data` is `Cro::TCP::Message.data`, read in `Cro::HTTP::ResponseParser`'s
`whenever $in -> Cro::TCP::Message $packet`. A raw `Buf` reaching that
subscription means the body bytes the parser emits into its **own**
`$raw-body-byte-stream` Supplier are being delivered back to the parser's
upstream `whenever $in` — supply cross-talk, not a chunked-decoding bug as such.
(The `Cro::TCP::Message` type constraint on the parameter is also not enforced,
which is why it surfaces as a missing method rather than a binding failure.)

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

The `ContentLength` parser has the same outer shape and works, so the difference
is either the nested `sub` or the fact that `Chunked` needs more than one
upstream packet.

## Smaller divergence found while narrowing (may or may not be the same bug)

This two-level shape already diverges without any Cro:

```raku
sub chunk-parser(Supply $raw) {
    supply {
        my $buffer = Buf.new;
        whenever $raw -> $blob { $buffer.append($blob); drain(); }
        sub drain() {
            while $buffer.elems >= 2 { emit $buffer.subbuf(0, 2); $buffer .= subbuf(2) }
        }
    }
}
class Msg { has $.data; }
sub transformer(Supply $in) {
    supply {
        my ($raw, $body-out);
        whenever $in -> Msg $packet {
            if !$raw.defined {
                $raw = Supplier.new;
                $body-out = chunk-parser($raw.Supply);
                emit $body-out;
            }
            $raw.emit($packet.data);
        }
    }
}
my $wire = Supplier.new;
my @bodies;
transformer($wire.Supply).tap(-> $b { @bodies.push($b) });
$wire.emit(Msg.new(data => Buf.new(0x61, 0x62)));
$wire.emit(Msg.new(data => Buf.new(0x63, 0x64)));
my @got;
@bodies[0].tap(-> $v { @got.push($v.decode('ascii')) });
$wire.emit(Msg.new(data => Buf.new(0x65, 0x66)));
say @got.raku;      # raku: ["ef"]   mutsu: []
```

The single-level version (a `supply` whose `whenever` calls a `sub` declared
after it) is clean in both, so the nesting is what breaks it: an inner supply
created inside an outer supply's `whenever` body loses the values later pushed
into its source.

## Note

Diagnosing this was much harder than it should have been because
`Promise($supply)` swallowed the real error and reported
`Impossible coercion from 'Any' into 'Promise'`. That is fixed separately
(`news/2026-08/coercion-method-errors-no-longer-swallowed.md`).
