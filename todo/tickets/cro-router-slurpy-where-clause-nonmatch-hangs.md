# `http-router.rakutest` hangs on a slurpy positional param with a failing `where` clause

Discovered while fixing `parameter-type-not-nominalized-for-user-subsets.md`
and the sibling `X::TypeCheck::Binding::Parameter.parameter` crash (both now
resolved) — those fixes let `t/http-router.rakutest` (vendored Cro::HTTP test
suite, `tmp/frameworks/cro/t/http-router.rakutest` locally) run 10 more
subtests than before, and it now hangs deterministically at test 181 instead
of dying earlier.

Route under test:

```
get -> 'content', *@path where *[*-1].ends-with('.html') {
    response.status = 200;
    ...
}
```

Request `/content/foo/bar.jpg` (expected: 404, since `bar.jpg` does not end in
`.html`) hangs mutsu indefinitely (confirmed at a 150s timeout, well beyond
CPU-contention noise — see `docs/flaky-test-policy.md` triage protocol; this
is not the known-flaky-timeout class).

A minimal isolated repro of the same shape (`-> *@path where
*[*-1].ends-with('.html') { ... }` invoked via `|$cap` unpacking with a
mismatching value, wrapped in `try { ... CATCH { when
X::TypeCheck::Binding::Parameter { .parameter } } }`) does **not** reproduce
the hang — it returns promptly with a proper `Parameter` object. So the hang
is not simply "where-constraint failure on a slurpy param"; it likely involves
Cro's own multi-candidate route-matching machinery (the router builds many
`get -> ...` handlers as alternative candidates and re-invokes a near-miss
candidate via `$imp(|cap)` from `@*BIND-FAILS` only when the overall route
match fails) or an interaction with the LTM/regex-alternation work tracked in
ADR-0022 (Slice 5 is still open).

To reproduce: `bash tmp/cro-t.sh t/http-router.rakutest` (needs the Cro
campaign vendored checkout under `tmp/frameworks/cro` /
`tmp/cro-work/inc-paths.txt` — see `handoff-cro-next-steps` session memory /
`docs/batteries/web-framework.md`). It reliably stops after `ok 180` with no
further output.

Next step: bisect with a repro that includes the full `route { get -> 'A' ...;
get -> 'content', *@path where ...; }` multi-candidate shape (not just a
single bare pointy block), since the isolated single-candidate repro does not
reproduce the hang.
