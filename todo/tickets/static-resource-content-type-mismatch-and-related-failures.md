# `t/http-router.rakutest` "resources" (bundled-module static assets) block: 10 content-type/status mismatches

Discovered the same way as
`for-loop-var-shared-across-nested-closure-captures.md`: fixing
`multipart-form-data-body-not-destructured-in-request-body-handler.md` let
`t/http-router.rakutest` (vendored Cro::HTTP suite) run to completion
(`1..439`) for the first time, surfacing tests that were previously
unreachable (the file used to die partway through on the multipart bug).
Tests 413-432 (the "resources" block, `t/http-router.rakutest` lines
~1960-2026, `static 't/samples/resources.rakumod', ...`-style routes backed
by a bundled/precompiled module's embedded resources) fail:

```
not ok 413 - Get index.html from resources
not ok 414 - resource sets correct status code
not ok 415 - resource sets correct content-type
not ok 416 - Get folder/test.txt from resources
not ok 417 - Good status
not ok 418 - Good content-type
not ok 419 - Get <folder test.txt> from resources
not ok 420 - Good status
not ok 421 - Good content-type
not ok 422 - indexes in a folder of resources
not ok 423 - Good status
not ok 424 - Good content-type
not ok 425 - indexes in root of resources, 1
not ok 426 - Good status
not ok 427 - Good content-type
not ok 428 - indexes in root of resources, 2
not ok 429 - Good status
not ok 430 - Good content-type
not ok 431 - The extension point for other plugins wanting to use resources works
not ok 432 - Good content-type
```

Example failure (test 432, `t/http-router.rakutest` line 2024):

```
# expected: 'text/html; charset=utf-8'
#      got: 'Cro::MediaType.new(type => "text", suffix => "", subtype-name => "html", tree => "", parameters => ().Seq)'
```

## What's ruled out so far

`is()`'s failure diagnostic renders a non-`Str` actual value via its
`.gist`/`.raku`-like form, which is what produced the `Cro::MediaType.new(...)`
text above — that is NOT proof of a stringification bug by itself.
Confirmed in isolation (`tmp/mediatype-str-repro2.raku`, no Cro::HTTP::Router
involved) that `Cro::MediaType.parse('text/html; charset=utf-8').Str` and
`... eq 'text/html; charset=utf-8'` both already work correctly and match
real `raku` byte-for-byte. So the bug is NOT in `Cro::MediaType`'s own
stringification/eq — `$r.content-type` in the actual response is most
likely holding a genuinely different `Cro::MediaType` value (e.g. missing
`charset`, wrong subtype, or the `text` block route/resource lookup itself
failing before content-type is even set) — not yet isolated further.

Every "Good status"/"... from resources" test in the same block also fails,
suggesting the underlying resource lookup/serving path itself may be
broken (wrong bytes, wrong status, not just wrong content-type header) —
next step is to check what `body-text($r)` and `$r.status` actually are for
test 413/414 (currently unknown; only the `content-type` mismatches were
excerpted above) before assuming this is a MediaType-only issue.

## Next step

1. Read `t/http-router.rakutest` lines ~1955-2026 for the exact routes
   (`static 'path', :resources` or similar — uses a bundled precompiled
   module's `%?RESOURCES` mechanism) and reproduce the block standalone via
   `bash tmp/cro-t.sh` with a trimmed copy, without the rest of the file.
2. Check what `$r.status` / `body-text($r)` actually come back as (not just
   `content-type`) — the ticket text above only captured the content-type
   diffs from the `make roast`-style log; the full picture (wrong file
   served vs. 404 vs. wrong headers only) is unknown.
3. If it turns out to be a `%?RESOURCES`/module-embedded-resource lookup bug
   (mutsu's module precompilation resource bundling, not Cro-specific),
   check `docs/mzef-install-pipeline.md` and existing resource-related
   tickets/tests for related mechanism.

Reproduce via `bash tmp/cro-t.sh t/http-router.rakutest` (see
`handoff-cro-next-steps` session memory for the Cro campaign scaffolding
under `tmp/cro-work/`).
