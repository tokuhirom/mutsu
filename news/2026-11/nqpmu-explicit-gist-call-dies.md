# An explicit `.gist`/`.raku` call on the `NQPMu` object dies

`my ::foo $x; $x.gist` answered because the VM's native `gist`/`raku` lanes ran before
the dispatcher's `NQPMu` gate. The early metarole render check that both method-call
paths already run now raises `X::Method::NotFound` for `gist`, `raku`, `perl`, `Str`,
`say`, `put` and `print` on an `NQPMu` type object, as rakudo does. Fixes #16369.
