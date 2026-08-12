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

## Update: `$r.status`/`body-text` checked — it's a 404, not a content-type bug

The routes are `sub resourcey-routes()` in `t/TestModule/lib/TestModule.rakumod`
(pulled in via `use lib 't/TestModule'; use TestModule;` at the top of
`http-router.rakutest`), not a `static 'path', :resources` form. Its `route {
resources-from %?RESOURCES; get -> 'index.html' { resource 'index.html'; }
... }` uses `Cro::HTTP::Router`'s `:resource-plugin` (`resources-from`/
`resource` subs, `lib/Cro/HTTP/Router.rakumod` around line 1530-1576).

Test 414 ("resource sets correct status code") actually reports:
```
# expected: '200'
#      got: '404'
```
So this is **not** a `Cro::MediaType`/content-type-stringification bug at
all (that ruling-out still stands — see above) — `sub resource` itself falls
through to its final `not-found;` (line 1575), meaning its `.{$path}` lookup
against the `%resources` hash passed to `resources-from %?RESOURCES` never
finds `'index.html'`.

**One related bug in this area is already found and fixed** (separate PR,
see `news/2026-08/with-given-resources-pseudo-var-element-topic.md`):
`with %?RESOURCES{$key} -> $resource {...}` (a shape used inside
`resolve-route-resource`/similar helper subs) used to silently bind
`$resource` to `Nil` because `with`/`given`'s element-source writeback
optimization mishandled the `%?RESOURCES` pseudo-var. **That fix did NOT
resolve this ticket's 404** — confirmed by re-running
`t/http-router.rakutest` after it landed; tests 413-432 still fail exactly
the same way. The 404 must come from a *different* mechanism: `resources-from
%?RESOURCES` passes the pseudo-hash as a **plain sub-call argument** (no
subscript, no `with`/`given`), and `resource`'s own lookup
(`with .{$path} -> $resource` inside a `for @resource-hashes { ... }` loop)
also isn't a bare `%?RESOURCES` reference — it's `.{$path}` called on
whatever `router-plugin-get-configs` returned, i.e. entirely downstream of
Cro's own `router-plugin-add-config`/`router-plugin-get-configs` config
storage (dynamic-variable-based plugin config registry, not `%?RESOURCES`
itself). The remaining bug is somewhere in that path — possibly:
(a) `%?RESOURCES` resolving to the WRONG (or empty) distribution when read
inside a `route { ... }` block that is itself the body of an exported sub
(`resourcey-routes`) called from a *different* package/frame than the one
`%?RESOURCES`'s `build_resources_for_package` package-resolution logic
expects (see the "Priority 1/2/3" comment in `src/runtime/run_dist.rs`), or
(b) a bug in `router-plugin-add-config`/`router-plugin-get-configs` itself
(a Cro mechanism, not core mutsu) losing/misreading the passed hash.

Attempting to isolate with a minimal non-vendored-TestModule fixture
(`use lib '...'; use Cro::HTTP::Router; sub my-routes() { route {
resources-from %?RESOURCES; get -> 'index.html' { resource 'index.html' } }
}`) hit a DIFFERENT, so-far-unexplained "Expected IO::Handle" error at
route-*definition* time (not request time) — not yet root-caused, and it's
unclear whether that's the same bug wearing a different face or an
artifact of the simplified fixture's setup (e.g. a difference from
`t/TestModule`'s exact META6.json/provides shape). Whoever picks this up
next should first reproduce that "Expected IO::Handle" error against the
REAL `t/TestModule` fixture (not a new simplified one) to rule out a
fixture-setup artifact, then narrow from there — likely with `CRODBG=1`
tracing through `router-plugin-add-config`/`get-configs`
(`Router.rakumod`), or `rust-gdb` breakpoints on
`build_resources_for_package` to check what distribution it resolves to
when called from inside `resourcey-routes`'s `route {}` block vs. from the
top-level test file.

## Next step

1. Reproduce the 404 (not content-type) directly against `t/TestModule`
   (the real fixture, already vendored under
   `tmp/cro-work/C_RO_CRO_HTTP_.../t/TestModule/`) with a trimmed script
   that skips the rest of `http-router.rakutest` — call `resourcey-routes()`
   and a single `/index.html` request, print `$r.status`.
2. Add temporary debug prints (or `CRODBG`-style tracing) inside a **local,
   throwaway copy** of `resources-from`/`resource` in the vendored
   `Router.rakumod` (`tmp/cro-work/` is gitignored scratch, safe to edit
   temporarily — never edit `roast/`) to see whether `%resources` arrives
   empty, or arrives correct but `.{$path}` fails for some other reason
   (wrong key format, `Slip` mishandling — see the `$io !~~ Slip` guard in
   `resource`'s own source).
3. Once isolated, decide whether the fix belongs in `build_resources_for_package`
   (`src/runtime/run_dist.rs`) or elsewhere.

Reproduce via `bash tmp/cro-t.sh t/http-router.rakutest` (see
`handoff-cro-next-steps` session memory for the Cro campaign scaffolding
under `tmp/cro-work/`).
