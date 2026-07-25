# todo/ — open findings, one file per finding

Discovered bugs and missing features that are too large to fix right now live
here, **one file per finding**, split into two directories by the *nature* of
the work:

- **`todo/tickets/`** — small, self-contained, well-scoped items. A "TICKETS.md
  style" slice: pick one up and finish it in a session (a missing method, a
  parser slice, a narrow compat gap). Low risk, no design needed.
- **`todo/deep/`** — deep, hard problems. High blast radius, multi-session,
  needs design or an ADR before touching (dual-store decoupling, GC, large
  refactors, gnarly semantics). Capture the analysis so a future session can
  pick it up cold.

One file per finding (`<kebab-slug>.md`). A brand-new file conflicts with
nothing on merge — that is the whole point. Appending these to PLAN.md collided
constantly across the many small parallel PRs, exactly the problem the
per-entry `news/YYYY-MM/` files already solved. So a finding is a new file,
never an edit to a shared list.

Splitting by directory lets you tally the backlog with no frontmatter and no
script: `ls todo/tickets/ | wc -l` and `ls todo/deep/ | wc -l`.

## Format

Each file: an H1 title and a prose body (no frontmatter — same as `news/`)
covering:

- **Root cause** — what actually goes wrong, and where.
- **Affected files** — the modules/paths involved.
- **Why it is large** — why it cannot be fixed in one sitting.
- **Repro** — a minimal script or roast path that exhibits it.

(A `deep/` entry naturally carries more analysis than a `tickets/` one.)

## Lifecycle

- **open** → a file under `todo/tickets/` or `todo/deep/`.
- **resolved** → `git mv` it to `news/YYYY-MM/<slug>.md` (flat, chronological)
  and rewrite it as an accomplishment.
- **evaporated / no longer real** → delete it.
- A `deep/` problem that turns out to be a quick fix can move to `tickets/`
  first, or just be fixed directly — the split is a guide, not a wall.

`todo/` holds only *open* findings. PLAN.md stays for planned strategic /
campaign work; ad-hoc discovered findings go here.

Roast per-test pass/fail status stays in its own ledger,
`TODO_roast/BLOCKERS.md` — do not duplicate roast tracking here. (There is no
special naming rule for roast; a genuinely deep, non-roast-specific problem you
happen to hit via a roast test can still get a `todo/deep/` file.)

## Do not file these as GitHub issues

Bug records stay in-repo as files. Do not open GitHub issues for them on your
own initiative — and never against any repository other than `tokuhirom/mutsu`
(an AI has mis-filed a mutsu issue against a Raku-org repo before).
