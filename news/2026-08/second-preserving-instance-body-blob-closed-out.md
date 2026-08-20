# The "second Supplier::Preserving body-blob returns empty" deep ticket was already fixed — closed out after re-verification

`todo/deep/second-preserving-instance-body-blob-returns-empty-in-same-supply-body.md`
(filed 2026-08-11) tracked the sole remaining failure in the vendored
Cro::HTTP2 `http2-request-parser.rakutest`: with two concurrently-open
HTTP/2 streams, the second stream's `.body-blob.result` resolved to an
empty `Buf` even though `emit`/`done` fired on the exact same
`Supplier::Preserving` object identity the reader held.

Re-verification on current `main` (2026-08-20) shows the bug no longer
reproduces: the full vendored test file passes 61/61, five consecutive
runs, including the previously-failing "Header1 + Header2 + Data1 + Data2"
check 4. The root cause was found and fixed on 2026-08-13 by the
fast-method-dispatch-cache fix
(`news/2026-08/fast-method-cache-drops-attributive-param-writeback.md`):
the `(class, method)`-keyed fast cache never mirrored attributive
parameters (`method set-body-byte-stream(Supply $!body-byte-stream)`) into
the instance's attribute cell, so the **second** `%streams{$sid}.message`
receiver's `$!body-byte-stream` silently stayed the bare `Supply` type
object — its `body-blob` then tapped the `else { supply { } }` fallback
and resolved with the untouched empty buffer. That is exactly the
"second instance loses its body" symptom this ticket chased; the ticket
just never got closed when the fix landed.

The re-verification did surface one adjacent, still-live deterministic
bug in the same machinery — `LAST done;` in a tapped supply block's
`whenever` escaping as an empty runtime error and double-firing the
downstream done — fixed in the same PR as this closeout, see
`news/2026-08/supply-whenever-last-done-signal-consumed.md`.
