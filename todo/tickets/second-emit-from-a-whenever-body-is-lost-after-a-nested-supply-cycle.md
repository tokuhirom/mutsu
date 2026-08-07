# A supply-block lexical written from a nested sub reverts between `whenever` invocations

`Cro::HTTP::RequestParser` parses two pipelined requests but only ever delivers
the first. Root-caused: the parser's `$request` lexical, reassigned by its nested
`sub fresh-message`, is **back to the previous object** on the next `whenever`
invocation, so the second request's headers are appended to the first request's
object.

## The failure chain

1. `fresh-message` runs and *does* assign a new request — a note inside the sub
   prints `new request Cro::HTTP::Request|906`.
2. The next `whenever $in` invocation sees `$request` as `Cro::HTTP::Request|767`
   again — the object from the *first* request.
3. Packet 2's headers are appended to it, so it ends up with
   `("Content-Type=text/plain", "Content-Length=22", "Content-Type=text/plain",
   "Content-Length=19")`.
4. Two `Content-Type` headers make `$request.content-type` parse
   `'text/plain,text/plain'`, which dies with `X::Cro::MediaType::Invalid`.
5. That exception escapes `emit $request`, so the second request never reaches
   the tap and the test times out.

`Cro::MediaType.parse('text/plain,text/plain')` fails identically under real
`raku`, so step 4 is correct behaviour reached from a wrong state — the bug is
entirely steps 1-2.

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

The vendored `Cro/HTTP/RequestParser.rakumod` carries `%*ENV<CRODBG>`-gated
`[DBG-P]` notes covering every step above — run the repro with `CRODBG=1` and the
whole chain is visible without re-instrumenting.

## Not a regression

Measured on `75b0ad4ca`, before the supply emitter-stamp campaign of
#6044/#6047/#6048: identical `got 1`. It predates that work. It also survives
#6048 (supply-block lexicals as shared `ContainerRef` cells), which fixed the
*read* side of the same family — sibling `whenever`s and `LAST` phasers now share
the block's lexical — so this is a distinct write-visibility hole.

## Reduced repros that do NOT reproduce it

Five shapes were tried and all behave correctly, so the next attempt should not
re-derive them (they are in `tmp/subwrite*.raku`):

- a nested `my sub` writing a supply-block scalar, read back on a later
  `whenever` invocation;
- the same where the sub is called both from the supply body and from the
  `whenever` body, allocating a fresh object each time;
- the sub called inside a `loop` in the `whenever` body, followed by `next`;
- the `whenever` body emitting the very object the sub then replaces;
- the same with a nested supply created, fed and completed in between.

The synthetic route is exhausted; per the project's own guidance the next step is
a **shadow bisect of the real file** — copy `Cro/HTTP/RequestParser.rakumod` into
a shadow tree that `-I` puts first, and delete statements from the `whenever`
body until the revert stops.

## Secondary defect found on the way

The escaping exception is reported as `X::AdHoc: X::Cro::MediaType::Invalid()` —
a typed exception flattened into `X::AdHoc` with the *type object's gist* as its
message. Caught directly (`try Cro::MediaType.parse('text/plain,text/plain')`)
the same exception keeps its class and message under mutsu, so the mangling is in
the supply/quit propagation path, not in `die`.

## Why it matters

3 of the 7 remaining failures in Cro's `t/http-request-parser.rakutest` ("Two
separate packages are parsed" and its two split-packet variants), and it means a
mutsu Cro server cannot serve pipelined requests on one connection.
