# A `whenever` body's `emit` is lost on its second invocation, after a nested supply completed

`Cro::HTTP::RequestParser` parses two pipelined requests but only ever delivers
the first. The second request is parsed completely — request line, headers, body
bytes — and then `emit $request` neither reaches the tap nor returns.

## Reproduction

```raku
# tmp/twopkg.raku
use Cro::HTTP::RequestParser;
use Cro::TCP;

sub to-tcp($text) {
    Cro::TCP::Message.new(data => $text.subst("\n", "\r\n", :g).encode('latin-1'));
}
my $m1 = "POST /bar HTTP/1.1\nContent-Type: text/plain\nContent-Length: 22\n\nFields, Flowers, Rails";
my $m2 = "POST /bar HTTP/1.1\nContent-Type: text/plain\nContent-Length: 19\n\nMountains and Seas\n";

my $parser = Cro::HTTP::RequestParser.new;
my $fake-in = Supplier.new;
my @got;
my $done = Promise.new;
$parser.transformer($fake-in.Supply).tap: -> $request {
    @got.push($request.body-text.result);
    $done.keep(True) if @got == 2;
};
start {
    $fake-in.emit(to-tcp($m1));
    $fake-in.emit(to-tcp($m2));
    $fake-in.done();
}
await Promise.anyof($done, Promise.in(5));
say "got {@got.elems}: ", @got.raku;
```

`raku` prints `got 2`, mutsu `got 1`.

## What the trace shows

The vendored `Cro/HTTP/RequestParser.rakumod` carries `%*ENV<CRODBG>`-gated
`[DBG-P]` notes for exactly this (run with `CRODBG=1`). For the **second**
packet every step runs:

```
[DBG-P] packet in, bytes=88 expecting=RequestLine
[DBG-P] req-line="POST /bar HTTP/1.1"
[DBG-P] header-line="Content-Type: text/plain"
[DBG-P] header-line="Content-Length: 19"
[DBG-P] header-line=""
[DBG-P] blank line; has-cl=True has-te=False req=Cro::HTTP::Request|767
[DBG-P] body-stream set, about to feed
[DBG-P] count=20
[DBG-P] fed body bytes
```

and then stops. The very next statement is `emit $request`, and the note placed
immediately after it never fires — so `emit` itself throws or does not return.
For the first packet the identical sequence works.

Two further observations from the same trace:

- `req=Cro::HTTP::Request|767` is the **same `.WHICH` for both requests**, even
  though `fresh-message` ran in between (`[DBG-P] leftover kept, elems=0` proves
  the `fresh-message; next;` branch was taken) and assigns
  `$request = Cro::HTTP::Request.new`. Either the assignment does not survive to
  the next `whenever` invocation or two distinct instances share a `.WHICH`.
- Between the two `emit $request` calls, the body byte stream is fed, which runs
  `Cro::HTTP::RawBodyParser::ContentLength`'s `whenever` to completion including
  its `done`. A nested supply completing mid-body is the obvious suspect for
  closing something the outer supply still needs.

## Not a regression

Measured on `75b0ad4ca` (before the supply emitter-stamp campaign of
#6044/#6047/#6048): identical `got 1`. It predates that work.

## Why it matters

It is 3 of the 7 remaining failures in Cro's `t/http-request-parser.rakutest`
("Two separate packages are parsed", and the two split-packet variants), and it
means a mutsu Cro server cannot serve pipelined requests on one connection.

## Reduced repros that do NOT reproduce it

Worth knowing so the next attempt does not re-derive them:

- a nested `my sub` writing a supply-block lexical, read back on a later
  `whenever` invocation — correct;
- the same where the sub is called both from the supply body and from the
  `whenever` body, allocating a fresh object each time — correct (each
  invocation sees the new object).

So the trigger needs the nested body-parser supply in the middle, not just the
nested sub.
