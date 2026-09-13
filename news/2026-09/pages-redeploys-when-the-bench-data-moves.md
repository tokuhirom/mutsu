# The Pages deploy follows the bench data, and stops calling itself a WASM demo

`.github/workflows/pages.yml` was still named **"Deploy WASM Demo to GitHub
Pages"**, a name from when `site/` really was one page with a Run button. It has
not been that for a long time — `site/README.md` opens by saying so outright
("It is **mutsu's home page**, not a WebAssembly demo") — and by now the
workflow also publishes the manual, the tutorial, the batteries listing, the
ecosystem parity page and the bench-trend dashboard. The workflow is now
**"Deploy the site to GitHub Pages"**.

## The real bug the rename was sitting on top of

`bench-trend.html` is rendered at deploy time from the `bench-data` branch, and
`bench.yml` appends a row there after every non-documentation push to `main`.
Nothing connected the two. Look at how that row arrives:

- `bench.yml` pushes to `bench-data` using the default `GITHUB_TOKEN`, and
  GitHub deliberately refuses to start a workflow from such a push;
- `bench-data` is not `main`, and `pages.yml` only watches `main` anyway;
- the data is not in the repository tree at all, so no `paths:` filter could
  ever match it.

So there was no push event to react to, not even in principle. The only thing
that ever refreshed the published trend was the nightly `schedule:` — which
meant a freshly measured commit could sit unpublished for **up to 24 hours**,
and the file's own comment listed "the bench-data history that bench-trend.html
renders" among the things the cron existed to paper over.

The fix is to react to the thing that *does* produce a signal: the Bench run
itself. `Bench` joins `Release` in the `workflow_run` trigger, so a completed
measurement redeploys the site within minutes.

This also closes a lag the render step used to document as unavoidable. When
the deploy was triggered by a `site/**` push, it raced `bench.yml` on the same
commit and reliably rendered a history that stopped one commit short. Triggered
by Bench's *completion*, the row for the commit just measured is already on
`bench-data` when the chart is drawn.

The job's `if:` guard had to learn the difference between the two watched
workflows. It previously demanded `workflow_run.event == 'push'`, which is
right for `Release` — a `workflow_dispatch` Release run uploads artifacts but
creates no release and publishes no npm package, so there is nothing new to
deploy — and wrong for `Bench`, which appends its row however it was started.
The guard now applies the push requirement to `Release` only.

## Ecosystem data needed nothing, and that is worth writing down

The obvious symmetric move — adding `Ecosystem sweep` to the same
`workflow_run` list — would have been a regression. A sweep does not write to a
side branch; it lands its records as a pull request into `main`, which the
existing `ecosystem/**` path filter already catches at exactly the right
moment. Firing on the sweep's completion instead would run the build while that
pull request was still open, checking out a tree that still holds the
*previous* sweep's numbers. The workflow now carries a comment saying so, so the
next reader does not have to re-derive it.
