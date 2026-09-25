# The mutsu site

The static site deployed to <https://tokuhirom.github.io/mutsu/> by
`.github/workflows/pages.yml`. It is **mutsu's home page**, not a WebAssembly demo:
the landing page introduces the implementation (what it is, how to install it, what is
inside), and then argues why the language it implements is worth a look. A hands-on
Raku tutorial, a playground and a REPL follow. Every code sample runs locally in the
visitor's browser through mutsu compiled to WebAssembly — nothing is sent to a server,
which makes the site both the pitch and the proof.

**The playground and the REPL are two pages, deliberately.** The playground answers
"what does this program print": an editor, a Run button, an output pane, and a fresh
interpreter for every run, exactly like running a file. The REPL answers "what is this
value, and what can I do with it next": one line at a time, in a session that keeps
every declaration, with a continuation prompt for unfinished lines. They used to share
one page — an editor whose Run button fed the REPL's transcript and its interpreter —
which made neither question easy to ask.

The site is deliberately **build-step free**: plain HTML, ES modules and CSS, served
as-is. The only generated input is `pkg/`, copied from the published
`@tokuhirom/mutsu` npm package by the Pages workflow.
Successful tagged releases trigger that workflow after npm publication, so the
deployed interpreter follows the package's `latest` dist-tag.

## Layout

```
index.html          landing page — what mutsu is, install, what is inside,
                    then "why Raku" with runnable highlights
tutorial.html       the tutorial: chapter/lesson navigation + one runnable lesson
playground.html     editor + Run + output: whole programs, one clean run each
repl.html           the interactive session: one line at a time, state kept
embed-demo.html     runnable installed-package demo and browser integration guide
internals.html      Internals hub: pipeline, value representation, VM, GC (prose)
opcodes.html        every VM opcode with operands, doc and cost — generated data
types.html          every Value kind + the built-in type tree — generated data
assets/
  site.css          all styling
  i18n.js           language selection, UI strings, shared nav + footer
  corpus.js         parser for the .txt snippet corpora (shared with Node)
  data.js           fetch for the generated content/ files: always revalidate,
                    never accept a stale cached copy (the file says why)
  highlight.js      the small Raku syntax highlighter
  editor.js         textarea + highlight overlay widget
  runner.js         WASM lifecycle: isolated runs and long-lived REPL sessions
  snippet.js        editor + Run + output + expected-output widget
content/
  lessons.txt       tutorial code and expected output (the corpus)
  highlights.txt    landing-page code and expected output
  examples.js       playground programs and REPL one-liners
  install.js        the install recipes (shell code, shared between languages)
  tutorial.en.js    tutorial titles and prose (English)
  tutorial.ja.js    tutorial titles and prose (Japanese)
  landing.en.js     landing-page copy (English)
  landing.ja.js     landing-page copy (Japanese)
  internals.en.js   Internals hub prose (English)
  internals.ja.js   Internals hub prose (Japanese)
  opcodes.json      VM opcode reference — generated, git-ignored (see below)
  types.json        value kinds + built-in type tree — generated, git-ignored
  stats.json        compatibility numbers — generated, git-ignored (see below)
bench-trend.html    benchmark dashboard — generated, git-ignored (see below)
pkg/                installed npm package — generated, git-ignored
```

## The compatibility headline

The landing page leads with "N% of the official spec files pass in full". That
number is **counted at deploy time**, not written into the copy: `pages.yml` divides
`roast-whitelist.txt` by the number of `.t` files under `roast/` and writes
`content/stats.json`. A local checkout has no such file, so the page falls back to
the figure baked into `index.html` — keep that fallback roughly current, but the
deployed site never depends on it. (The README's hand-written count is exactly what
this avoids: it sat ~290 files stale.)

Counting it at deploy time is only half the guarantee: the deploy has to happen.
Neither `roast-whitelist.txt` nor `roast/` was on the workflow's `paths:` filter, so
for a while this figure moved several times a day and reached the site once a night.
`pages.yml` now deploys on **every** push to main, which is affordable because the
job no longer builds wasm from source — it installs the published npm package, and
runs in ~35 seconds.

`bench-trend.html` is rendered into the site at deploy time by
`scripts/bench-visualize.py` from the `bench-data` branch's `bench-history.tsv`.
A completed `Bench` run re-triggers the deploy, so a new measurement reaches the
published trend within minutes instead of waiting for the nightly run: `bench.yml`
pushes to `bench-data` with the default `GITHUB_TOKEN`, and such a push cannot
start a workflow, so the run's completion is the only signal the new data exists.
Passing `--site-chrome` makes it load `assets/site.css` and `assets/i18n.js` and
render the same nav, language switch and footer as every other page, so the shared
chrome has exactly one definition. Without the flag the script keeps producing a
fully self-contained file for offline use:

```sh
git show origin/bench-data:bench-history.tsv \
  | python3 scripts/bench-visualize.py --standalone --site-chrome -o site/bench-trend.html
```

## The Internals section

`internals.html` explains how mutsu works inside, for contributors: the
pipeline, the NaN-boxed `Value`, the bytecode VM and the cycle collector. It is
hand-written prose, and deliberately contains no *lists*. The lists live on two
reference pages whose data is read out of the source at deploy time by
`scripts/gen-internals-manifest.py`:

| Page | Data | Read from |
| --- | --- | --- |
| `opcodes.html` | `content/opcodes.json` | `enum OpCode` in `src/opcode.rs` (operands, `///` docs, `// -- Section --` families) and the `// Cost:` line above each arm of `exec_one_dispatch` in `src/vm/vm_exec_dispatch.rs` |
| `types.html` | `content/types.json` | `enum Kind` + `payload_op` in `src/value/nanbox/mod.rs` (each kind's payload and whether it is inline, `Arc`, `Gc` or `WeakGc`) and `CATALOG` in `src/builtins/builtin_type_catalog.rs` (MROs and roles) |

Both JSON files are git-ignored, like `stats.json`: `pages.yml` generates them
for the deploy and ci.yml's `wasm-e2e` job generates them before the e2e test,
so the published reference always describes the commit it came from. The
script fails instead of writing an empty listing when the source layout it
parses has changed. To preview locally:

```sh
python3 scripts/gen-internals-manifest.py --summary   # --summary lists doc/Cost gaps
```

Improving the reference means improving the source: a `///` doc comment on an
`OpCode` variant or a `Kind` shows up on the page at the next deploy.

## Languages

The site is bilingual (English / Japanese). The picked language lives in
`localStorage` and in the `?lang=` query parameter, so a shared link keeps the
language it was read in. Switching re-renders in place; nothing reloads.

**Code is shared between languages, prose is not.** The corpus files hold one copy of
each snippet, keyed `<chapter>/<lesson>`; the per-language modules supply titles and
explanations for those same keys. That way a snippet can never drift between the two
translations, and adding a language means adding prose only.

## View state lives in the URL

A page whose controls change what it shows records that state in
`location.hash`, so the view on screen can be linked, bookmarked and survive a
reload:

| Page | Hash |
| --- | --- |
| `bench-trend.html` | `#metric=seconds\|ratio\|instr\|allocs` · `window=0\|50\|150` (default 150) · `y=fit\|zero` · `view=charts\|table` · `sort=<column>` · `dir=asc\|desc` |
| `ecosystem.html` | `#q=<search text>` · `status=<status>` |
| `tutorial.html` | `#<chapter>/<lesson>` |
| `playground.html` | `#code=<encoded program>` |

Two rules keep the URL trustworthy, and `e2e.test.mjs` covers both:

- **Defaults are left out**, so an untouched page keeps a bare URL and a link
  names only what was actually chosen.
- **The URL never describes a page that is not on screen.** A value the page
  cannot honour — `metric=instr` against a history with no deterministic
  series, a status this corpus does not contain — is ignored *and* rewritten
  out of the hash, rather than left there claiming a filter is applied.

Writes go through `history.replaceState`, not `location.hash = …`: clicking
through four metrics is one page, not four entries to back out of. `hashchange`
is still handled, so a hand-edited URL, and a step across a real history entry,
both re-render.

## The snippet corpora

`content/lessons.txt` and `content/highlights.txt` use one format:

```
#== basics/hello
say "Hello, World!";
#-- expect
Hello, World!
```

The `#-- expect` blocks are **generated, not hand-written**:

```sh
cargo build
node scripts/check-site-snippets.mjs --update
```

That runs every snippet under `target/debug/mutsu` *and* under `raku`, and records an
expectation only when the two agree — so the tutorial can only teach behaviour real
Raku actually has, and a snippet that mutsu gets wrong is caught while it is being
written rather than by a reader. Without `--update` the script checks instead of
writing and exits non-zero on drift; CI runs it that way (`raku` is not on the
runner, so there it is a pure regression gate).

The tutorial shows the recorded output under "Expected output", compares a run
against it, and marks the lesson done in the table of contents when they match.

A snippet may carry flags after its key:

```
#== concurrency/promises no-browser
```

`no-browser` means "runs natively, but not in the WebAssembly build" — the snippet
needs something a browser cannot provide at all (spawning a real process, say). Those
lessons are still checked natively by `check-site-snippets.mjs`; the site disables
their Run button, explains why, and shows the recorded native output instead of a WASM
trap. No lesson currently carries the flag.

**Concurrency is not one of those cases.** `start`, `await`, `Promise`, `Channel`,
`Thread`, `Supply.interval` and `sleep` all run in the browser, on the cooperative
scheduler in `src/runtime/wasm_sched.rs`: a would-be thread becomes a task on a run
queue, and every point that would block on another thread pumps that queue instead.
It is concurrency without parallelism — nothing runs at the same time as anything
else, and a task that blocks midway on something only its waiter would do later
reports a deadlock rather than hanging the tab. `site/concurrency.test.mjs`
pins the behaviour (run it with Node, no browser needed).

## Adding a lesson

1. Add a `#== <chapter>/<id>` block to `content/lessons.txt` at the position it should
   appear (chapter order follows first appearance).
2. Add a title and prose under the same key to **both** `content/tutorial.en.js` and
   `content/tutorial.ja.js`.
3. `node scripts/check-site-snippets.mjs --update` to record the output.
4. `node site/e2e.test.mjs` to check it in the browser.

Nothing else needs touching: the table of contents, the counters, prev/next, and the
e2e lesson sweep are all derived from the corpus.

## Running locally

```sh
npm install --prefix tmp/pages-package @tokuhirom/mutsu
mkdir -p site/pkg
cp -R tmp/pages-package/node_modules/@tokuhirom/mutsu/. site/pkg/
python3 -m http.server 8000 -d site    # then open http://localhost:8000/
```

Use `scripts/build-npm-package.sh` and copy its `pkg/` directory to `site/pkg/`
instead when testing an unpublished local package change.

## Tests

```sh
node scripts/check-site-snippets.mjs   # every snippet, under mutsu (+ raku if present)
node site/concurrency.test.mjs    # start/await/Channel/timers in the WASM build
npm install playwright && npx playwright install chromium
node site/e2e.test.mjs            # the site itself, in a real browser
SKIP_LESSON_SWEEP=1 node site/e2e.test.mjs   # skip the per-lesson sweep
```

The e2e suite runs **every** tutorial lesson in the browser and compares against the
recorded expectation, so a WASM-only regression cannot reach the deployed site
unnoticed. It also renders `bench-trend.html` from a synthetic history and checks its
chrome, so the generated page cannot silently drift away from the hand-written ones.

## Credit

The tutorial and its examples follow the official Raku documentation
([Raku/doc](https://github.com/Raku/doc)), vendored in this repository as `raku-doc/`
and used under the Artistic License 2.0. The credit is shown in the site footer on
every page.
