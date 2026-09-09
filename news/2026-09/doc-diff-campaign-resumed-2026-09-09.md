# doc-diff campaign resumed: a full re-sweep on current main, and 14 tickets from it

The doc-diff campaign was restarted on 2026-09-09 with a fresh full-corpus
sweep against current `main` (`a44bd28`, through PR #7740) and `raku` v2026.07 —
443 files, `-j4`, about 35 minutes on a 4-core remote container. This is the
first sweep to run with #7590's output cap and twice-run oracle gate in place,
so it is also the first one whose raw output could be committed without hand
truncation.

## What the numbers say

| sweep | match | mismatch | crash | drift | nondet dropped |
|---|---:|---:|---:|---:|---:|
| 2026-09-06 | 2376 | 74 | 34 | 119 | — |
| 2026-09-07b | 2402 | 59 | 28 | 114 | — |
| **2026-09-09** | **2409** | **129** | **21** | *(retired)* | **59** |

The `mismatch` column looks like a large regression and is not one. #7590
retired the `raku-drift-from-doc` bucket, which had been absorbing every
divergence whose doc annotation raku itself no longer matched — a bucket only
reachable *after* mutsu already differed from raku, and 67 of whose 114 blocks
were confirmed real mutsu bugs. Those findings now land in `output-mismatch`,
where they always belonged. Read against the sum of the old buckets, the sweep
moved the right way: `match` 2402 → 2409, crashes 28 → 21, and 59 blocks that
would previously have been compared (and diverged on an unreproducible token
forever) were dropped by the oracle gate instead.

All three tickets filed from the 2026-09-07b sweep are fixed and closed
(#7591/#7578/#7587), so the ledger's "Ticketed" table started this round empty.

## What was filed

Every row below was reduced to a minimal repro and re-run against `raku`
v2026.07 before filing. Fourteen tickets, ordered roughly by severity:

- **#7746** — a `FatRat` addition **panics the process**: `add_sub.rs:168`
  narrows FatRat parts to `i64` and multiplies them unchecked, while the
  big-parts branch directly above it does the right thing.
- **#7747** — a `but`-mixin on a `Hash` silently loses the hash's contents on
  the next store, and the `%`-assign form corrupts the keys outright
  (`{2\t3 => (Any)}`).
- **#7748** (`todo:deep`) — `Proxy.new(FETCH/STORE)` bound to a name is not
  assignable, so the documented type is unusable end to end.
- **#7749** — adjacent colonpairs (`:a1:b2:c3`) stop parsing after the first.
- **#7750** — runtime binding failures are reported with the *compile-time*
  "Calling f(...) will never work with declared signature" wrapper, and one
  case throws `X::TypeCheck::Argument` where raku throws
  `X::TypeCheck::Binding::Parameter`. Four `signatures.rakudoc` rows at once.
- **#7751** — `does PositionalBindFailover` is ignored, so such an object
  cannot bind to a `@a` parameter.
- **#7752** — `%%` and `%` by zero throw eagerly where raku returns a soft
  `Failure`; `div` already does the right thing, which is what makes this a
  routing fix rather than a new mechanism.
- **#7753** — a `...` sequence passed straight to a builtin listop collapses:
  `join` sees one element and `sum` sees none, while the method form, a user
  slurpy and `gather` all flatten it correctly.
- **#7754** — native integer increment does not wrap (`my int $x = 2**63-1;
  ++$x` promotes to a big `Int`), even though native assignment already
  truncates and unsigned subtraction already wraps.
- **#7755** — junctions do not flatten through infix `~` (a junction of
  junctions instead of one junction), and do not autothread out of a list into
  `join`.
- **#7756** — `IO::CatHandle`: `.encoding: Nil` ignored, `.words` merges the
  last word of one handle into the first of the next, empty-cat `.slurp`
  prints nothing instead of `Nil`.
- **#7757** — a parametric role with **named** parameters never matches
  (`role R[:$v]`), through either `but` or `does`.
- **#7758** — a sub-signature on a **named** parameter is ignored: every
  sub-parameter is bound to the whole array.
- **#7759** — `.bless(value => …)` does not fill a `Str` subclass's payload;
  only `.new` does.

#7759 is the one worth calling out as a process finding. The backlog listed
`objects.rakudoc:1067` under *Resolved*, citing
`news/2026-08/str-subclass-loses-native-stringify.md`. The fix was real but
covered only the `Mu.new` path, while the doc example reaches the payload
through `self.bless(...)` — and with no `t/` pin on the `bless` spelling,
nothing said so. The Resolved entry has been corrected in place rather than
deleted, so the over-claim stays visible.

## Also refreshed

- `docs/doc-diff-sweep/` now holds the 2026-09-09 raw data: `summary.txt`,
  `progress.txt`, and the 80 signal-file reports (500 KB, capped by the
  harness). Its `README.md` carries the exact refresh commands, including the
  one-liner that regenerates the survey table.
- `docs/doc-diff-backlog.md` has a new Corpus snapshot with the
  bucket-comparability warning, a Ticketed table pointing at the fourteen
  issues, a re-triaged "real, not yet filed" cluster list, an explicit
  "environment noise, not findings" note for the `$*DISTRO`/`$*VM`/REPL rows,
  and a regenerated 80-row survey.
