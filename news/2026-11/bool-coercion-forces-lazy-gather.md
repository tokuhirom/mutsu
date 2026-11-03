# `ok find(...)`: a `Bool()` parameter forces a lazy gather Seq

A `Bool(Mu)`/`Bool()` parameter coerced a not-yet-run `gather` Seq with
`Value::truthy`, which cannot pull, so it read as False. Test's `proclaim` takes
`Bool(Mu) $cond`, so `ok find(...)` (File::Find returns `gather while ...`) failed
where `so find(...)` passed (Distribution::Extension::Updater's `t/01-basic.rakutest`).
The coercion now reads the Seq's first element first, as the VM's other boolean
chokepoints do. The nested-watcher test this unmasked passes in CI as well.
