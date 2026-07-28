# The precompilation cache no longer drops what parsing left behind

`src/precomp.rs` caches a module's `Vec<Stmt>` on disk and, on a hit,
`parse_module_source` returned it and skipped the parse. That is sound only if
parsing is a pure `source -> AST` function. In mutsu it is not: the parser also
writes thread-local state that the rest of the runtime reads afterwards, and a
cache hit performed none of those writes. The result was a class of bug where
the same program produced different results depending on whether
`~/.cache/mutsu/precomp` happened to be warm — and, because the effect only
appears from the *second* run onwards, CI could never see it.

Two effects were measured to matter and are now stored in the cache entry
(`ParseEffects`) and replayed on a hit:

- **The language revision** the module's `use vX` selected. Without it, code
  running while the module's mainline executed saw the *importer's* revision.
  This is how `roast/S14-roles/versioning.t` came to pass on a cold cache and
  fail on every run after it, with a role group's candidates all collapsing to
  one revision (see [role-candidate-language-revision](role-candidate-language-revision.md)).
- **Parse warnings**, which were reported on the first run of a program and then
  silently vanished on every later one.

A third candidate, inline `module Foo { ... is export }` registrations, was
measured and behaves identically cold and warm — the importer's own uncached
export scan already registers them — so it is documented as a deliberate
non-entry rather than guessed at.

Three smaller defects in the same file went with it:

- **Entries could not name themselves.** The cache file name is a 64-bit hash of
  the canonical source path, but the path was not stored, so an entry was trusted
  purely because it sat at the expected name. The canonical path is now part of
  the metadata and is verified on load.
- **The cache grew without bound.** `clear_cache()` was dead code and nothing
  ever evicted an entry whose module stopped being loaded; one real checkout had
  accumulated 12,355 files. Entries past a 4096 cap are now evicted oldest-first,
  at most once per process and only from the save path.
- **There was no way to turn the cache off from outside the CLI.** `--no-precomp`
  only reaches interpreters built by `main.rs`, so a test harness or CI step
  could not exercise the no-cache path. `MUTSU_PRECOMP=0` now disables it
  process-wide.

## The warm path is under test now

`t/precomp-warm-cache-parity.t` runs a probe program twice against a private
`XDG_CACHE_HOME` and requires byte-identical stdout *and* stderr. The probe
module is `use v6.e.PREVIEW` and computes `sprintf('%#x', -256)` in its mainline
(`-0x100` under 6.e, `0x-100` under 6.d) and carries a duplicated `is export`
trait so its parse warns — i.e. it fails on both counts without the replay,
which was confirmed by removing the replay and watching the warm run print
`0x-100` with no warning.

CI additionally re-runs the `S10-packages` / `S11-modules` / `S14-roles` slice of
the whitelist (46 files, ~12s) *after* `make roast`, when the cache the full run
just built is warm. Before this, every roast step began with
`rm -rf ~/.cache/mutsu/precomp` and runners were fresh, so the hit path had no
coverage at all.
