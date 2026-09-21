# The Pages deploy runs on every push, now that it stopped building wasm

`.github/workflows/pages.yml` had a `paths:` filter — `site/**`, `modules/**`,
`ecosystem/**`, two generator scripts and itself — and a comment explaining that `src/**` was
*deliberately* left out, because "nearly every commit touches it, which would restore the every-merge
build".

That reasoning expired. The job used to build the wasm playground from source; it does not any more.
The interpreter now comes from the published npm package (`npm install @tokuhirom/mutsu@latest`), and
what remains is a node install plus four small generator scripts. Measured end to end over six
consecutive runs on 2026-09-21: **30, 35, 36, 37, 41 and 47 seconds**.

At 35 seconds the filter costs more than it saves, because every path it omits is a way for the
deployed site to disagree with `main`:

- **`roast-whitelist.txt` was on no push path at all.** It is the numerator of the landing page's
  "N% of the official spec files pass in full", counted at deploy time precisely so the figure cannot
  drift from the suite — and it grows several times a day. The count was fresh; the *deploy* that
  published it was only the nightly cron, so the headline figure could sit up to 24 hours behind.
- **`roast/` was not either**, and it is the denominator of the same figure, so a roast re-vendor
  moved it with no trigger.
- Anything a future generator script starts reading is now covered from the moment it is written,
  instead of the first time someone notices a stale page and remembers to extend a list in this file.

So the filter is gone: every push to main deploys.

## What this does not fix, and the one thing to watch

The playground still runs the interpreter from the last published npm package, not from HEAD.
Deploying more often cannot change that — only a release can, and the `Release` workflow_run trigger
already picks that up. The old comment's "the playground runs the interpreter from the last deploy,
not from HEAD" was the honest consequence of a *build* this job no longer does.

The burst case is handled by the `concurrency` group that was already there
(`group: pages`, `cancel-in-progress: true`). When several PRs merge within a minute — routine in this
repo; four landed inside 60 seconds on the day of this change — the superseded runs are cancelled and
only the last one deploys. That is what makes an unfiltered trigger safe rather than a way to queue
twenty deploys of one tree.

The thing to watch is GitHub's Pages deployment rate limiting. Bursts collapse, but a steady stream of
merges a few minutes apart for hours could push the deploy count per hour higher than it has ever been
here. The failure mode is benign — a red deploy run, with the next push or the nightly cron publishing
the same content — so this is worth watching rather than pre-solving. If it does start failing, the
fix is a debounce (a `workflow_run` on CI completion, or a cron every N minutes), not a return to
guessing which paths the site reads.

The daily schedule stays, now as the safety net for what no push can signal: an npm package published
outside a tagged release, and a stretch of days with no merges at all.
