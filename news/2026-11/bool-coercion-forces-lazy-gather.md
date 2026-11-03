# `ok find(...)`: a `Bool()` parameter forces a lazy gather Seq, and a qualified type may be declared later

Distribution::Extension::Updater's `t/01-basic.rakutest` is 18/18 like rakudo after two fixes:

- A `Bool(Mu)`/`Bool()` parameter coerced a not-yet-run `gather` Seq with
  `Value::truthy`, which cannot pull, so it read as False. Test's `proclaim` takes
  `Bool(Mu) $cond`, so `ok find(...)` (File::Find returns `gather while ...`) failed
  where `so find(...)` passed. The coercion now reads the Seq's first element first,
  as the VM's other boolean chokepoints do.
- The post-declared-type check refused a qualified name (`X::Directory::FileExists`)
  used in a method before the file declares it, so `use Directory` died. rakudo
  resolves a qualified name when the code runs; only a single-segment name is a
  compile-time "Illegally post-declared type".
