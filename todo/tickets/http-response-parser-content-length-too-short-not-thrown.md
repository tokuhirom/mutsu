# `Cro::HTTP::ResponseParser` "Connection close with incomplete body throws" check 4 still fails after ADR-0028

## Context

ADR-0028 Slice 1 (`Supply.schedule-on()` genuine tap deferral) fixed the
deadlock that made `http-response-parser.rakutest` abort mid-run. Re-running
the full file post-fix (release build,
`tmp/cro-work/C_RO_CRO_HTTP_*/t/http-response-parser.rakutest`) now completes
end to end: 155/156 ok, down from the pre-fix 154/156 (which never actually
reached the end of the file before the deadlock's 10s `Promise` guard timed
it out early). The one remaining failure is narrower and unrelated to
scheduling:

```
not ok 116 - check 4
# Failed test 'check 4'
# at .../t/http-response-parser.rakutest line 31
```

This is subtest 4 of the `parses 'Connection close with incomplete body
throws', ...` block (`t/http-response-parser.rakutest:296-309`):

```raku
parses 'Connection close with incomplete body throws',
    q:to/RESPONSE/,
    HTTP/1.1 200 OK
    Content-length: 1000

    Far too short
    RESPONSE
    *.http-version eq '1.1',
    *.status == 200,
    *.headers == 1,
    {
        try await .body-text;
        $!.isa(X::Cro::HTTP::RawBodyParser::ContentLength::TooShort)
    }
```

i.e. when a response declares `Content-length: 1000` but the connection
closes after delivering far fewer bytes, `.body-text` should throw
`X::Cro::HTTP::RawBodyParser::ContentLength::TooShort`
(`lib/Cro/HTTP/RawBodyParser.rakumod:19,44` in the vendored Cro checkout) and
`$!` should be that exception. In mutsu it apparently doesn't (or throws a
different exception) — not yet root-caused.

## Reproduce

```
DIST=$(echo /home/tokuhirom/work/mutsu-roast/tmp/cro-work/C_RO_CRO_HTTP_*)
INC=$(cat /home/tokuhirom/work/mutsu-roast/tmp/cro-work/inc-paths.txt)
timeout 60 /home/tokuhirom/work/mutsu-roast/target/release/mutsu $INC -I "$DIST/lib" -I "$DIST/t" "$DIST/t/http-response-parser.rakutest"
```

Requires the vendored Cro checkout under `tmp/cro-work/` from prior sessions
(not part of this repo's tracked test suite — Cro itself is intentionally
not bundled, see `handoff-cro-next-steps` project memory / `PLAN.md`).

## Next step

Not yet investigated. Start by isolating a Cro-free repro: a `RawBodyParser`
(or whatever the equivalent generic content-length-driven body-consuming
supply chain is in mutsu) fed fewer bytes than `Content-length` declares,
followed by connection close, checking whether the expected exception type
reaches the awaiting `.body-text` caller at all, or reaches it as a
different exception/value. Compare against real `raku` per the repo's
investigation-order convention (`CLAUDE.md` "Investigating a failing roast
test").
