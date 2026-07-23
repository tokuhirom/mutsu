# wasm-demo — the mutsu site

The static site deployed to <https://tokuhirom.github.io/mutsu/> by
`.github/workflows/pages.yml`. It is an introduction to **Raku**, not just a demo of
mutsu: a landing page arguing why the language is interesting, a hands-on tutorial,
and the playground. Every code sample runs locally in the visitor's browser through
mutsu compiled to WebAssembly — nothing is sent to a server.

The site is deliberately **build-step free**: plain HTML, ES modules and CSS, served
as-is. The only generated input is `pkg/`, the `wasm-pack` output.

## Layout

```
index.html          landing page — "why Raku", with runnable highlights
tutorial.html       the tutorial: chapter/lesson navigation + one runnable lesson
playground.html     editor + persistent REPL (this used to be index.html)
assets/
  site.css          all styling
  i18n.js           language selection, UI strings, shared nav + footer
  corpus.js         parser for the .txt snippet corpora (shared with Node)
  highlight.js      the small Raku syntax highlighter
  editor.js         textarea + highlight overlay widget
  runner.js         WASM lifecycle: isolated runs and long-lived REPL sessions
  snippet.js        editor + Run + output + expected-output widget
content/
  lessons.txt       tutorial code and expected output (the corpus)
  highlights.txt    landing-page code and expected output
  tutorial.en.js    tutorial titles and prose (English)
  tutorial.ja.js    tutorial titles and prose (Japanese)
  landing.en.js     landing-page copy (English)
  landing.ja.js     landing-page copy (Japanese)
bench-trend.html    benchmark dashboard — generated, git-ignored (see below)
pkg/                wasm-pack output — generated, git-ignored
```

`bench-trend.html` is rendered into the site at deploy time by
`scripts/bench-visualize.py` from the `bench-data` branch's `bench-history.tsv`.
Passing `--site-chrome` makes it load `assets/site.css` and `assets/i18n.js` and
render the same nav, language switch and footer as every other page, so the shared
chrome has exactly one definition. Without the flag the script keeps producing a
fully self-contained file for offline use:

```sh
git show origin/bench-data:bench-history.tsv \
  | python3 scripts/bench-visualize.py --standalone --site-chrome -o wasm-demo/bench-trend.html
```

## Languages

The site is bilingual (English / Japanese). The picked language lives in
`localStorage` and in the `?lang=` query parameter, so a shared link keeps the
language it was read in. Switching re-renders in place; nothing reloads.

**Code is shared between languages, prose is not.** The corpus files hold one copy of
each snippet, keyed `<chapter>/<lesson>`; the per-language modules supply titles and
explanations for those same keys. That way a snippet can never drift between the two
translations, and adding a language means adding prose only.

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
reports a deadlock rather than hanging the tab. `wasm-demo/concurrency.test.mjs`
pins the behaviour (run it with Node, no browser needed).

## Adding a lesson

1. Add a `#== <chapter>/<id>` block to `content/lessons.txt` at the position it should
   appear (chapter order follows first appearance).
2. Add a title and prose under the same key to **both** `content/tutorial.en.js` and
   `content/tutorial.ja.js`.
3. `node scripts/check-site-snippets.mjs --update` to record the output.
4. `node wasm-demo/e2e.test.mjs` to check it in the browser.

Nothing else needs touching: the table of contents, the counters, prev/next, and the
e2e lesson sweep are all derived from the corpus.

## Running locally

```sh
wasm-pack build --target web --no-default-features --features wasm
mv pkg wasm-demo/pkg
python3 -m http.server 8000 -d wasm-demo    # then open http://localhost:8000/
```

## Tests

```sh
node scripts/check-site-snippets.mjs   # every snippet, under mutsu (+ raku if present)
node wasm-demo/concurrency.test.mjs    # start/await/Channel/timers in the WASM build
npm install playwright && npx playwright install chromium
node wasm-demo/e2e.test.mjs            # the site itself, in a real browser
SKIP_LESSON_SWEEP=1 node wasm-demo/e2e.test.mjs   # skip the per-lesson sweep
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
