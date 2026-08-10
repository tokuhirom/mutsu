# HTTP/2 Data frame content check ("check 4") fails consistently across `http2-request-parser`/`http2-request-serializer`/`http2-response-serializer` rakutest suites

## TL;DR

Three Cro::HTTP2 rakutest files each have exactly one recurring subtest
failure shape: a test helper (`sub test($request, $count, $desc, *@checks,
:$fail)` in `http2-request-serializer.rakutest`, similarly named/shaped
helpers in the other two files) runs several numbered "check N" assertions
per emitted HTTP/2 frame; the LAST check in each affected case — comparing
the frame's `.data` (a `Buf`, the DATA-frame body payload) against the
originally-emitted random `Buf` via `eq` — fails, while earlier checks in
the same case (frame type, flags, stream-identifier) pass.

As of 2026-08-10 (release build, `bash tmp/cro-suite-run.sh http`):

- `http2-request-parser.rakutest`: 1 failure (subtest "check 4")
- `http2-request-serializer.rakutest`: 3 failures (same "check 4" shape,
  once per test-case with a body: "Header + Data", "Header + Data (no
  content-length)", possibly a third variant — see the file)
- `http2-response-serializer.rakutest`: 3 failures, identical shape

## Evidence

```
DIST=<CRO_HTTP_CHECKOUT>
$BIN $INC -I "$DIST/lib" -I "$DIST/t" t/http2-request-serializer.rakutest
# ok 3 - check 3
# not ok 4 - check 4
# (repeated 3x across the file's test cases)
```

Relevant excerpt, `<CRO_HTTP_CHECKOUT>/t/http2-request-serializer.rakutest`:

```raku
sub test($request, $count, $desc, *@checks, :$fail) {
    ...
    $serializer.transformer($fake-in.Supply).tap:
    -> $frame {
        for @checks[$counter].kv -> $i, $check {
            ok $check($frame), "check {$i + 1}";
        }
        ...
    }, ...
    ...
}
...
test $req, 2, 'Header + Data',
    [[(* ~~ Cro::HTTP2::Frame::Headers), (*.flags == 4),
      (*.stream-identifier == 5), (*.headers eq $encoder.encode-headers(@headers))],
     [(* ~~ Cro::HTTP2::Frame::Data), (*.flags == 1),
      (*.stream-identifier == 5), (*.data eq $random)]];   # <-- check 4 of the 2nd frame: fails
```

The body flows: `$body = Supplier::Preserving.new; $body.emit: $random;
$body.done; ... $req.set-body-byte-stream: $body.Supply;` → serialized by
`Cro::HTTP2::RequestSerializer.transformer` (`<CRO_HTTP_CHECKOUT>/lib/Cro/HTTP2/RequestSerializer.rakumod:51-66`):

```raku
if $req.has-body {
    with $req.header('Content-Length') {
        my $counter = $_;
        whenever $body-byte-stream {
            $counter -= .elems;
            emit Cro::HTTP2::Frame::Data.new(
                flags => $counter == 0 ?? 1 !! 0,
                stream-identifier => $req.http2-stream-id,
                data => $_
            );
            ...
        }
    }
    ...
}
```

So the emitted `Frame::Data.data` should be byte-identical to `$random`
(a single 123-byte chunk, well under any frame-size limit, emitted once).

## What was tried and ruled out (do not re-attempt — both matched `raku`,
neither reproduced the mutsu bug)

1. A minimal `Buf eq` round-trip through `Supplier::Preserving` (no HTTP2
   involved): `$body.emit($random); $body.done; $body.Supply.tap: -> $data
   { say $data eq $random }` — printed `True` under both `raku` and mutsu.
   See `tmp/buf-eq-repro.raku`.
2. A minimal nested-`whenever`-with-implicit-topic repro mirroring the
   RequestSerializer's exact shape (outer `whenever $in -> $body-byte-stream
   { whenever $body-byte-stream { emit Frame.new(data => $_) } }`, no `Cro`
   dependency) — printed `True`/correct elem counts under both `raku` and
   mutsu. See `tmp/whenever-data-topic-repro.raku`.
3. A third variant adding the exact `with $req.header('Content-Length') {
   my $counter = $_; whenever $body-byte-stream { ... $_ ... } }` topic
   shape (outer `with`'s `$_` captured to `my $counter` BEFORE a nested
   `whenever` reuses `$_` for its own, different topic — the leading
   candidate hypothesis, since this project's campaign has repeatedly found
   `$_`/topic-leak-through-frame bugs, see MEMORY.md's "直近で入った一般バグ
   修正" note, 4 such bugs in one session) — also printed correct output
   under both `raku` and mutsu. See `tmp/whenever-data-topic-repro2.raku`.

None of the three isolates the trigger, so the bug needs either the real
`Cro::HTTP2::RequestSerializer`/`ResponseSerializer` class (with its full
`Cro::Transform` role composition, `HTTP::HPACK::Encoder`, and the rest of
`Cro::HTTP2::Frame`'s class hierarchy) or a closer structural match not yet
found — most likely something in `Cro::Transform`'s own `.transformer()`
composition/dispatch machinery, or `Cro::HTTP2::Frame::Data`'s own
attribute/`is`-trait machinery, rather than the bare `whenever`/topic
mechanics tested above. A real shadow-bisect of
`Cro::HTTP2::RequestSerializer.rakumod` itself (or `rust-gdb` breakpoints
tracing what `$_`/the `Data.new(data => ...)` argument actually holds at
each `emit`) is the next step, not another from-scratch synthetic repro.

## Discovery context

Found during a Cro::HTTP suite re-measurement (2026-08-10, `bash
tmp/cro-suite-run.sh http`, 26/35 files fully green). This is one of the
remaining 9 non-green files.

## Verification (once fixed)

- `http2-request-serializer.rakutest`, `http2-response-serializer.rakutest`,
  `http2-request-parser.rakutest` should each report `notok=0` in
  `tmp/cro-suite-run.sh http`'s per-file summary.
