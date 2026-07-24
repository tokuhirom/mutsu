# Bundle the HTTP client's dependency layer

`HTTP::UserAgent` — the HTTP-client battery mutsu is heading for — declares six
runtime dependencies beyond the already-bundled `IO::Socket::SSL`. Their upstream
test suites were run against mutsu first, before any bundling decision, and the
result was better than expected: five of them pass **completely**, unmodified.

Those five are now vendored into `modules/` and resolve with zero config
(`use URI;` — no `-I`, no `zef install`, no network):

| Module | Version | License | Upstream suite |
| --- | --- | --- | --- |
| `URI` | 0.3.8 | Artistic-2.0 | 14/14 |
| `MIME::Base64` | 1.2.5 | Artistic-2.0 | 4/4 |
| `HTTP::Status` | 0.0.5 | Artistic-2.0 | 3/3 |
| `DateTime::Parse` | 0.9.3 | MIT | 3/3 |
| `File::Directory::Tree` | 0.2 | Artistic-2.0 | 1/1 |

Every one of those 25 files is registered in `batteries.lock` /
`batteries-whitelist.txt`, so the release-time gate re-runs them against the
shipped library on every release and a regression blocks the release. The gate is
now 43/43. Nothing was patched into the vendored sources — the genuine community
dists run on mutsu as-is, which is the point (BATTERIES.md §1).

Two dependencies are deliberately **not** bundled yet, for unrelated reasons:

- **`Encode`** passes its full suite (7/7) but ships **no license at all** — no
  `LICENSE` file, no `license` key in `META6.json`, no statement in the README or
  sources. BATTERIES.md §4 makes a compatible license a hard gate, so it cannot be
  redistributed inside mutsu until upstream states one. This is an
  upstream-contribution task, not an interpreter one.
- **`File::Temp`** is Artistic-2.0 and 2/3 of its suite passes; the third file
  (`t/03-tempfile`) loads the module through
  `'use File::Temp; &tempfile, &tempdir'.EVAL`, and a module loaded inside `EVAL`
  loses its file-scoped helper subs once the `EVAL` returns
  (`Unknown function: make-temp`). That is a real interpreter bug; `File::Temp`
  is bundled once it is fixed, so the gate stays all-green.

The selection record — including why satisfying these dependencies natively or via
`mzef install` was rejected, the provenance table, and the re-vendoring recipe —
is `docs/batteries/http-deps.md`. `t/http-deps-battery.t` pins the zero-config
resolution and a smoke slice of each module's API.
