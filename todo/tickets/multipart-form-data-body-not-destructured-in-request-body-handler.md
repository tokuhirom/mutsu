# `request-body -> (:$name!, :$surname!) {...}` doesn't match against a parsed `multipart/form-data` body

Discovered while re-measuring `t/http-router.rakutest` (vendored Cro::HTTP
suite) after fixing `cro-router-slurpy-where-clause-nonmatch-hangs.md`,
`named-parameter-user-subset-type-not-enforced-at-binding.md`, and
`request-body-pair-signature-match-picks-wrong-block.md` — those three
fixes together got the file's first 360 subtests to 360/360, but the file
still exits 1: a final, unplanned block (starting at line 1555 of
`t/http-router.rakutest`) that spins up a real `Cro::HTTP::Server` +
`Cro::HTTP::Client` round trip dies on its very first request.

Route under test:

```raku
post -> 'destructure' {
    request-body -> (:$name!, :$surname!) {
        content 'text/plain', "Hello, $name $surname!";
    }
}
```

```raku
given await Cro::HTTP::Client.post("$base/destructure",
                                   content-type => 'multipart/form-data',
                                   body => [name => 'John', surname => 'Doe']) -> $resp {
    is await($resp.body-text), 'Hello, John Doe!',
        'multipart/form-data is handled with destructuring';
}
```

mutsu: the `Cro::HTTP::Client.post(...)` call itself throws
`X::Cro::HTTP::Error::Client` (the client's standard behavior for any 4xx
response), killing the rest of the file's `given` blocks (including the
sibling `application/x-www-form-urlencoded` and `application/json` variants
right after it, and a final `GET /test/uri` check) since none of them are
wrapped in `try`/`CATCH`.

## Diagnosis so far (CRODBG=1 trace)

`bash tmp/cro-t.sh t/http-router.rakutest` with `CRODBG=1` (see
`Cro::HTTP::Router.rakumod`'s `%*ENV<CRODBG>`-gated `note` calls) shows:

- The route matches correctly (`routing-outcome ... = (0, \("destructure"))`).
- The handler is invoked (`RouteHandler.invoke`).
- The handler's own body executes and **decides to return status 400 itself**
  (`[DBG-R] invoke-internal start block returning status 400`) — this is
  `request-body`'s `run-body-handler` sub in `Router.rakumod` reaching its
  final `die X::Cro::HTTP::Router::NoRequestBodyMatch.new;` because
  `$handler.signature.ACCEPTS(\(body))` returned `False` for the
  destructuring block `-> (:$name!, :$surname!) {...}`.

So this is **not** a router-matching bug (unlike the three tickets above,
already fixed) — the route itself matches fine. The failure is inside
`request-body`'s own dispatch: either

1. the **multipart/form-data body parser** doesn't produce a Hash shaped
   like `{name => 'John', surname => 'Doe'}` from the raw multipart bytes
   (e.g. wrong keys, wrong nesting, or an empty/malformed parse), or
2. the parser produces the right shape but `Signature.ACCEPTS` still
   rejects it for some other reason not yet isolated.

## Next step

Isolate with a Cro-independent repro:
1. First check whether the `application/x-www-form-urlencoded` and
   `application/json` sibling cases (the two `given` blocks immediately
   after the failing one, same destructuring handler) pass — if they do,
   the bug is specific to Cro's `Cro::HTTP::BodyParser::MultiPartFormData`
   (or wherever the vendored suite implements multipart parsing), not the
   destructuring/`ACCEPTS` mechanism itself (which was just fixed for the
   general sibling-`where`-clause case in
   `request-body-pair-signature-match-picks-wrong-block.md` — this ticket
   is a different bug even though it looks superficially similar).
2. If so, write a minimal repro that parses a raw
   `multipart/form-data; boundary=...` byte body directly through
   whatever `Cro::HTTP::BodyParser` class handles it, independent of the
   router/server round trip, and compare its resulting Hash shape against
   what `-> (:$name!, :$surname!)` expects.

To reproduce via Cro: `bash tmp/cro-t.sh t/http-router.rakutest` (see
`handoff-cro-next-steps` session memory for the Cro campaign scaffolding
under `tmp/cro-work/`) — the failure is the very last thing the file does
(after all 360 counted `ok` lines), so `CRODBG=1 bash tmp/cro-t.sh
t/http-router.rakutest` (see `Router.rakumod`'s debug notes) is the fastest
way to watch it happen without needing to wait through the whole file.
