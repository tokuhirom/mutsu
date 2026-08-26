# `Log::Async` is bundled as the general application-logging battery

mutsu now ships a conventional line-oriented logger. No `zef install`, no `-I`:

```raku
use Log::Async <trace>;
trace 'starting up';
error 'connection refused';
```

`Log::Async` v0.0.17 (`auth<zef:bduggan>`, Artistic-1.0-Perl) won this slot's
survey in `docs/batteries/logging.md` on ecosystem standing (17 dependents, more
than 3x the next candidate) and maintenance activity (released 2026-03-09,
against 2021-2024 for the rest of the field). It is vendored verbatim at
`modules/Log-Async/`, together with its one runtime dependency
`Terminal::ANSI` v0.0.25 (MIT) at `modules/Terminal-ANSI/` — which itself
depends only on the already-bundled `OO::Monitors`, so the slot added no new
dependency surface. This is a distinct slot from the already-bundled
`Log::Timeline`, which records task/event timelines rather than log lines.

## What unblocked it

The survey recorded this candidate as "Selected, not yet bundled" because
`Terminal::ANSI` ships `Terminal::ANSI::Virtual.rakumod` as
`unit monitor Terminal::ANSI::Virtual;` — the file-scope form of a declarator
keyword registered through `EXPORTHOW::DECLARE`, which mutsu's parser did not
accept. Fixing that (rung 2: grow the interpreter, never patch the vendored
module — see `news/2026-08/unit-form-exporthow-declare-keyword.md`) was the
whole gate.

Measured before and after, from each dist's own directory against a release
build:

| Suite | raku | mutsu (before) | mutsu (after) |
| --- | --- | --- | --- |
| `Log::Async` v0.0.17 | 17/17 files | 2/17 | **11/17** |
| `Terminal::ANSI` v0.0.25 | 8/8 files | 2/8 | **5/8** |

Both are pinned at that per-file baseline in `batteries-whitelist.txt` and
gated by `scripts/battery-testsuite.sh`, the same partial-coverage shape
`CBOR::Simple` and `Log::Timeline` ship in. The six `Log::Async` and three
`Terminal::ANSI` files that still fail are ordinary interpreter gaps, none in
the core log-a-message path — the largest is `logger.send-to($io-path)` dying
with `Invalid IO::Handle`, which alone accounts for two files and leaves the
`:formatter` API untested. They are enumerated in `docs/batteries/logging.md`.

## What shipped with it

- `modules/Log-Async/` and `modules/Terminal-ANSI/` (vendored `lib/` +
  `META6.json` + `README` + `LICENSE` where upstream ships one; `Log::Async`
  states its license only in `META6.json`, which is therefore the preserved
  license text for that dist).
- `batteries.lock` rows pinned to `c238fc01` (GitHub) and `691d0959`
  (SourceHut — a bare-sha `git fetch --depth 1` works there too, verified).
- `t/log-async-battery.t`, which drives all five severity levels through a
  custom `add-tap` sink and checks the message shape and the ordered severity
  enum. It passes identically under `raku` and mutsu.
- The refreshed `site/content/batteries.json` manifest and the
  `BATTERIES.md` §7 row, promoted from "Selected, not yet bundled" to Adopted.
