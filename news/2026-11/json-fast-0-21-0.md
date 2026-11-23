# The bundled JSON::Fast is re-vendored at 0.21.0

`modules/JSON-Fast/` now carries upstream `JSON::Fast:ver<0.21.0>:auth<zef:timo>`
(git tag `0.21`, commit `a06a1f49`), copied verbatim from the fez release, up
from 0.20.1.

The release makes the decoder "RakuAST ready": the whitespace and comment
skippers (`nom-ws`, `nom-comment`) no longer pass `++$pos` / `--$pos` to an
`int $pos is rw` parameter but an explicit
`$pos = nqp::add_i($pos, 1)` / `nqp::sub_i`, which RakuAST compiles to a native
assignment. Behaviour is unchanged. `batteries.lock` pins the new commit and
the upstream suite stays whole in the release gate. These two subs are on the
hot path of every decode, so the `#8673` decode benchmarks are re-baselined on
this version.

One local test changed with it. `t/modules/batteries/json-jsonc.t` asserted that
`from-json("[1] // trailing", :allow-jsonc)` returns `[1]`, which mutsu did
under 0.20.1 but rakudo never did. Under 0.21.0 mutsu and rakudo agree that a
line comment running into end of input without a newline is "additional text"
(the module's comment skipper steps one past the end), so the test now pins
that, plus the newline-terminated and block-comment shapes that both accept.
