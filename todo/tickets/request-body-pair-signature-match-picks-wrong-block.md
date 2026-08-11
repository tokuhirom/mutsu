# `request-body "type" => -> (:$x, :$y where ...) {...}, ...` picks the wrong candidate block

Discovered while re-measuring `t/http-router.rakutest` (vendored Cro::HTTP
suite) after fixing `cro-router-slurpy-where-clause-nonmatch-hangs.md`: test
327 ("request-body matches by signature (Pair case)") fails.

Route under test (`t/http-router.rakutest` around line 1141):

```raku
get -> 'bysig' {
    request-body
        "application/json" => -> (:$x, :$y where $y > $x) {
            content 'text/plain', "bysig($y > $x)";
        },
        -> (:$x, :$y where $y <= $x) {
            content 'text/plain', "bysig($y <= $x)";
        };
}
```

Request body `{"x":42,"y":101}` (so `$y > $x`, i.e. `101 > 42`) is expected
to match the first block ("bysig(101 > 42)") but mutsu picks/produces output
consistent with the second/fallback block instead — expected body
`bysig(101 > 42)`, actual differs (see `not ok 327` in the roast-adjacent
Cro run).

Not yet diagnosed against a Cro-independent minimal repro (Cro's
`request-body` multi-candidate dispatch layers `Cro::HTTP::BodyParserSelector`
+ block-signature `ACCEPTS` matching against a decoded JSON hash, so the
first attempt at isolating this should try `Signature.ACCEPTS`/direct call
against a `-> (:$x, :$y where $y > $x) {...}` block with a `Capture` built
from a decoded-JSON-shaped hash, independent of Cro and HTTP).

To reproduce via Cro: `bash tmp/cro-t.sh t/http-router.rakutest` (see
`handoff-cro-next-steps` session memory for the Cro campaign scaffolding
under `tmp/cro-work/`), test 327 (sibling test 326, "Block case", passes).
