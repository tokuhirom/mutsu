# The battery gate reads upstream test suites from a cache in CI

`scripts/battery-testsuite.sh` fetches each bundled library's upstream test suite
at the commit pinned in `batteries.lock`. On 2026-09-24 git.sr.ht, the only
non-GitHub host in the lock (`Terminal::ANSI`), was unreachable from GitHub
runners for long stretches. `test-suites` failed with `GATE ERROR` on unrelated
PRs, even after #9279 added four retries per fetch.

The script now takes an optional `BATTERY_SRC_CACHE` directory. A checkout for
a pinned commit is reused from `<dir>/<commit>` when it is there, and every
fresh fetch is stored there right after checkout, before any test runs in it.
Entries for commits the lock no longer pins are dropped at startup. The commits
are pinned, so a cached checkout is exactly what a fetch would produce. The
gate runs the same tests and is not weakened.

`ci.yml`'s `test-suites` job restores the directory with `actions/cache` before
the gate, keyed on `hashFiles('batteries.lock')` with a prefix `restore-keys`
fallback. It saves the directory afterwards, even when the gate fails, and
skips the save on an exact hit. PRs read the cache that `main` saves, so an
upstream host only has to be reachable once per pinned commit. That fetch
happens when a battery is re-vendored. `release.yml`'s `batteries` job restores
the same cache read-only, so an upstream outage does not block a release either.

Verified locally with a two-battery scratch lock:

1. A networked run filled the cache and pruned a stale entry.
2. A run with every https remote rewritten to an invalid host passed from the
   cache.
3. The same blocked run without the cache still failed with `GATE ERROR`.

Refs #9275.
