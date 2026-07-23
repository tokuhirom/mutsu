// E2E tests for the static site (landing page, tutorial, playground) using Playwright.
// Run: node wasm-demo/e2e.test.mjs
//
// Requires: npm install playwright
// Also requires wasm-demo/pkg/ to be built:
//   wasm-pack build --target web --no-default-features --features wasm
//   mv pkg wasm-demo/pkg
//
// The tutorial sweep runs EVERY lesson in the browser and compares against the
// expectation recorded in content/lessons.txt (which scripts/check-site-snippets.mjs
// generated from a native mutsu run cross-checked against raku). Set
// SKIP_LESSON_SWEEP=1 to skip it while iterating locally.

import { chromium } from 'playwright';
import { spawn, spawnSync } from 'child_process';
import { existsSync, readFileSync, writeFileSync, rmSync } from 'fs';

import { parseCorpus } from './assets/corpus.js';
import landingEn from './content/landing.en.js';
import INSTALL from './content/install.js';

const PORT = 18765;
const BASE = `http://localhost:${PORT}`;
let server;
let browser;
let passed = 0;
let failed = 0;

function assert(condition, message) {
  if (condition) {
    console.log(`  PASS: ${message}`);
    passed++;
  } else {
    console.error(`  FAIL: ${message}`);
    failed++;
  }
}

/** Feed one line to the REPL and return the text it logged (may be ''). */
async function replEval(page, line) {
  const before = await page.locator('#repl-log > div').count();
  await page.fill('#repl-input', line);
  await page.press('#repl-input', 'Enter');
  // evalLine runs synchronously inside the keydown handler, so by the time the
  // press resolves the log already has the echo (+ output, when there is any).
  const after = await page.locator('#repl-log > div').count();
  if (after <= before + 1) return '';   // echo only: no output (e.g. continuation)
  return (await page.locator('#repl-log > div').last().textContent()).trim();
}

/** Run the editor buffer and return the text it logged. */
async function runEditor(page, code) {
  await page.fill('#code', code);
  await page.click('#run-btn');
  await page.waitForFunction(() => !document.getElementById('run-btn').disabled,
                             { timeout: 30000 });
  return (await page.locator('#repl-log > div').last().textContent()).trim();
}

/** Press a snippet's Run button and return its output text. */
async function runSnippet(page, scope = '') {
  const btn = `${scope} .run-btn`.trim();
  await page.click(btn);
  await page.waitForFunction(
    (sel) => { const b = document.querySelector(sel); return b && !b.disabled; },
    btn, { timeout: 60000 });
  return (await page.locator(`${scope} .output`.trim()).first().textContent()).trim();
}

// Check pkg exists
if (!existsSync('wasm-demo/pkg/mutsu.js')) {
  console.error('Error: wasm-demo/pkg/ not found. Build WASM first.');
  process.exit(1);
}

const lessons = parseCorpus(readFileSync('wasm-demo/content/lessons.txt', 'utf8'));
const highlights = parseCorpus(readFileSync('wasm-demo/content/highlights.txt', 'utf8'));

// The bench dashboard is generated at deploy time from the bench-data branch.
// Render one here from a synthetic history so its site chrome is covered by the
// same suite as the hand-written pages — it is a page of the site, and it is
// easy to forget when the shared chrome changes.
const BENCH_HTML = 'wasm-demo/bench-trend.html';
const BENCH_TSV = 'wasm-demo/.bench-history.e2e.tsv';
const benchRows = [
  'date\tcommit\tbenchmark\tmutsu_median_s\tmutsu_min_s\traku_median_s\tratio_mutsu_over_raku\truns\trunner\trakudo',
];
for (let i = 0; i < 6; i++) {
  const sha = `${i}`.repeat(40);
  for (const [name, base] of [['bench-fib', 0.2], ['bench-hash', 0.04]]) {
    const t = (base * (1 + i / 50)).toFixed(4);
    benchRows.push(`2026-07-${10 + i}T00:00:00Z\t${sha}\t${name}\t${t}\t${t}\t0.3000\t0.80\t7\tci\tRakudo`);
    benchRows.push(`2026-07-${10 + i}T00:00:00Z\t${sha}\t${name}+jit\t${t}\t${t}\t0.3000\t0.70\t7\tci\tRakudo`);
  }
}
writeFileSync(BENCH_TSV, benchRows.join('\n') + '\n');
const rendered = spawnSync('python3', ['scripts/bench-visualize.py', '--standalone',
                                       '--site-chrome', BENCH_TSV, '-o', BENCH_HTML],
                           { encoding: 'utf8' });
if (rendered.status !== 0) {
  console.error('Error: could not render the bench dashboard:', rendered.stderr);
  process.exit(1);
}

// Start HTTP server
server = spawn('python3', ['-m', 'http.server', String(PORT), '-d', 'wasm-demo'], {
  stdio: ['ignore', 'pipe', 'pipe'],
});
await new Promise(r => setTimeout(r, 1500));

try {
  browser = await chromium.launch();
  const page = await browser.newPage();

  const errors = [];
  page.on('pageerror', err => errors.push(err.message));

  /* =============================================================== *
   * Landing page
   * =============================================================== */

  console.log('Test: landing page');
  await page.goto(`${BASE}/index.html?lang=en`, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => document.body.dataset.ready === '1', { timeout: 30000 });

  assert((await page.textContent('#hero-title')).length > 0, 'the hero has a title');
  assert(await page.locator('#why-cards .card').count() === highlights.length,
         `every highlight snippet has a card (${highlights.length})`);
  assert(await page.locator('#feature-cards .card').count() === landingEn.features.cards.length,
         `every feature has a card (${landingEn.features.cards.length})`);
  assert(await page.locator('#stat-row .stat').count() === 3,
         'the headline numbers are shown');
  assert(/^\d+(\.\d+)?%$/.test((await page.textContent('#stat-row .stat-value')).trim()),
         'the roast figure renders as a percentage');
  assert(await page.locator('.site-nav .nav-links a').count() >= 4, 'the nav lists the pages');
  assert((await page.textContent('.site-footer')).includes('Raku/doc'),
         'the footer credits the official Raku documentation');

  console.log('Test: install recipes');
  assert(await page.locator('#install-tabs button').count() === INSTALL.length,
         `every install recipe has a tab (${INSTALL.length})`);
  const firstRecipe = (await page.textContent('#install-panels pre')).trim();
  assert(firstRecipe === INSTALL[0].code.trim(),
         `the ${INSTALL[0].id} recipe is shown first`);
  await page.click(`#install-tabs button:nth-child(${INSTALL.length})`);
  assert((await page.textContent('#install-panels pre')).trim()
           === INSTALL[INSTALL.length - 1].code.trim(),
         'picking another tab swaps in its recipe');
  await page.click('#install-tabs button:nth-child(1)');

  console.log('Test: landing page language switch');
  const enTitle = await page.textContent('#hero-title');
  await page.click('.lang-switch button[data-lang="ja"]');
  const jaTitle = await page.textContent('#hero-title');
  assert(enTitle !== jaTitle, 'switching to Japanese re-renders the hero');
  assert((await page.textContent('.site-footer')).includes('Artistic License 2.0'),
         'the Japanese footer keeps the licence credit');
  assert(new URL(page.url()).searchParams.get('lang') === 'ja',
         'the language lands in the URL so links stay shareable');
  await page.click('.lang-switch button[data-lang="en"]');
  assert(await page.textContent('#hero-title') === enTitle, 'switching back restores English');

  console.log('Test: landing page snippet runs');
  const firstHighlight = highlights[0];
  const cardOut = await runSnippet(page, `#card-${firstHighlight.id}`);
  assert(cardOut === firstHighlight.expect,
         `the "${firstHighlight.key}" card produces its expected output`);
  assert(await page.locator(`#card-${firstHighlight.id} .verdict.ok`).count() === 1,
         'a matching run is marked as matching');

  /* =============================================================== *
   * Benchmark dashboard — a generated page, but still a page of the site
   * =============================================================== */

  console.log('Test: benchmark dashboard wears the site chrome');
  await page.goto(`${BASE}/bench-trend.html?lang=en`, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => document.querySelector('.site-nav a') !== null,
                             { timeout: 15000 });
  assert(await page.locator('.site-nav .nav-links a').count() >= 4,
         'it has the shared navigation');
  assert(await page.textContent('.site-nav a[aria-current="page"]') === 'Benchmarks',
         'with itself marked as the current page');
  assert((await page.textContent('.site-footer')).includes('Raku/doc'),
         'and the shared footer credit');
  assert(await page.locator('.lang-switch button').count() === 2,
         'and the language switch');
  assert(await page.locator('.bench-card svg').count() === 2,
         'the charts still render (one per benchmark)');
  await page.click('#view button[data-v="table"]');
  assert(await page.locator('#tableWrap tbody tr').count() === 2,
         'and the table view still works');
  assert(await page.evaluate(() =>
    getComputedStyle(document.body).backgroundImage.includes('gradient')),
    'it uses the site background rather than its own');
  assert(await page.evaluate(() =>
    document.documentElement.scrollWidth <= document.documentElement.clientWidth),
    'and does not scroll sideways');

  /* =============================================================== *
   * Tutorial
   * =============================================================== */

  console.log('Test: tutorial page');
  await page.goto(`${BASE}/tutorial.html?lang=en`, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => document.body.dataset.ready === '1', { timeout: 30000 });

  assert(await page.locator('.toc .lesson-link').count() === lessons.length,
         `the table of contents lists every lesson (${lessons.length})`);
  assert(await page.locator('.toc .chapter').count() ===
         new Set(lessons.map(l => l.group)).size, 'chapters are grouped');
  assert(await page.textContent('#lesson-title') !== '', 'the first lesson renders');
  assert(await page.locator('#prev-btn').isDisabled(), 'Previous is disabled on lesson 1');

  console.log('Test: tutorial navigation');
  await page.click('#next-btn');
  assert(new URL(page.url()).hash === `#${lessons[1].key}`,
         'Next moves to the second lesson and records it in the URL');
  const secondTitle = await page.textContent('#lesson-title');
  await page.click('#prev-btn');
  assert(await page.textContent('#lesson-title') !== secondTitle, 'Previous goes back');

  console.log('Test: tutorial deep link');
  const deep = lessons.find(l => l.group === 'regex');
  await page.goto(`${BASE}/tutorial.html?lang=en#${deep.key}`, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => document.body.dataset.ready === '1', { timeout: 30000 });
  assert((await page.inputValue('.editor-wrap textarea')).trim() === deep.code.trim(),
         'a deep link opens that lesson with its code loaded');
  assert(await page.locator('details.expected').count() === 1,
         'the lesson shows its expected output');

  console.log('Test: tutorial language switch keeps your edits');
  await page.fill('.editor-wrap textarea', 'say "mine";');
  const titleBefore = await page.textContent('#lesson-title');
  await page.click('.lang-switch button[data-lang="ja"]');
  assert(await page.textContent('#lesson-title') !== titleBefore,
         'the lesson title switches to Japanese');
  assert(await page.inputValue('.editor-wrap textarea') === 'say "mine";',
         'switching language does not discard what you typed');
  await page.click('.lang-switch button[data-lang="en"]');
  await page.click('.reset-btn');

  console.log('Test: tutorial marks a lesson done when its output matches');
  await runSnippet(page);
  assert(await page.locator('.toc .lesson-link.done').count() >= 1,
         'a matching run marks the lesson done in the table of contents');
  await page.click('#clear-progress');
  assert(await page.locator('.toc .lesson-link.done').count() === 0, 'progress can be cleared');

  console.log('Test: a lesson the WASM build cannot run says so');
  const blocked = lessons.find(l => l.flags.includes('no-browser'));
  if (blocked) {
    await page.evaluate(k => { location.hash = k; }, blocked.key);
    await page.waitForFunction(() => !document.querySelector('.snippet-note').hidden,
                               { timeout: 10000 }).catch(() => {});
    assert(!(await page.locator('.snippet-note').isHidden()),
           `${blocked.key} explains why it cannot run in the browser`);
    assert(await page.locator('.run-btn').isDisabled(), 'and its Run button is disabled');
    assert((await page.textContent('.output')).trim() === blocked.expect,
           'and it shows the output recorded from a native run');
    await page.evaluate(k => { location.hash = k; }, deep.key);
    await page.waitForFunction(() => document.querySelector('.snippet-note').hidden,
                               { timeout: 10000 });
    assert(!(await page.locator('.run-btn').isDisabled()),
           'moving to a runnable lesson re-enables Run');
  }

  console.log('Test: tutorial edits are runnable');
  await page.fill('.editor-wrap textarea', 'say "edited";');
  const editedOut = await runSnippet(page);
  assert(editedOut === 'edited', `an edited lesson runs (got: ${JSON.stringify(editedOut)})`);
  assert(await page.locator('.verdict.differs').count() === 1,
         'output that differs from the expectation says so');
  await page.click('.reset-btn');
  assert((await page.inputValue('.editor-wrap textarea')).trim() === deep.code.trim(),
         'Reset restores the lesson code');

  /* =============================================================== *
   * Every lesson actually runs in the browser
   * =============================================================== */

  if (!process.env.SKIP_LESSON_SWEEP) {
    const runnable = lessons.filter(l => !l.flags.includes('no-browser'));
    console.log(`Test: all ${runnable.length} browser-runnable lessons produce their expected output`);
    let sweepFailures = 0;
    for (const lesson of runnable) {
      // Navigate by hash rather than reloading: one page load means one WASM
      // instance for the whole sweep instead of ~20 MB recompiled per lesson.
      await page.evaluate(k => { location.hash = k; }, lesson.key);
      await page.waitForFunction(
        (code) => document.querySelector('.editor-wrap textarea').value.trim() === code,
        lesson.code.trim(), { timeout: 10000 });
      const out = await runSnippet(page);
      if (out !== lesson.expect) {
        sweepFailures++;
        console.error(`  ${lesson.key}\n    expected: ${JSON.stringify(lesson.expect)}` +
                      `\n    got:      ${JSON.stringify(out)}`);
      }
    }
    assert(sweepFailures === 0,
           `every lesson matches its recorded output (${sweepFailures} mismatched)`);
  }

  /* =============================================================== *
   * Playground
   * =============================================================== */

  console.log('Test: playground WASM initialization');
  await page.goto(`${BASE}/playground.html`, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => document.body.dataset.ready === '1', { timeout: 30000 });
  assert(!(await page.isDisabled('#repl-input')), 'REPL input is enabled once WASM is ready');

  // --- Test: REPL evaluates an expression and shows its value ---
  console.log('Test: REPL expression value');
  assert(await replEval(page, '1 + 2') === '3', 'bare expression shows its value');
  assert(await replEval(page, 'say "hi"') === 'hi', 'say prints its argument');

  // --- Test: REPL state persists across lines (the whole point) ---
  console.log('Test: REPL session state');
  await replEval(page, 'my $x = 40');
  assert(await replEval(page, '$x + 2') === '42', 'a variable declared earlier is still in scope');
  await replEval(page, 'sub double($n) { $n * 2 }');
  assert(await replEval(page, 'double(21)') === '42', 'a sub declared earlier is callable');

  // --- Test: multi-line continuation ---
  console.log('Test: multi-line continuation');
  assert(await replEval(page, 'if True {') === '', 'an unbalanced line produces no output');
  assert(await page.textContent('#repl-prompt') === '*', 'the prompt switches to the continuation marker');
  assert(await replEval(page, '  say "inside" }') === 'inside', 'the buffered block runs once closed');
  assert(await page.textContent('#repl-prompt') === '>', 'the prompt returns to normal');

  // --- Test: history recall ---
  console.log('Test: history');
  await page.press('#repl-input', 'ArrowUp');
  assert((await page.inputValue('#repl-input')).includes('say "inside"'),
         'ArrowUp recalls the previous line');
  await page.fill('#repl-input', '');

  // --- Test: editor Run shares the REPL's interpreter ---
  console.log('Test: editor/REPL shared session');
  const classOut = await runEditor(page, 'class Point { has $.x; method twice { $.x * 2 } }\nsay "declared";');
  assert(classOut === 'declared', `editor Run prints its output (got: ${JSON.stringify(classOut)})`);
  assert(await replEval(page, 'Point.new(x => 21).twice') === '42',
         'a class defined in the editor is usable from the REPL');
  assert(await replEval(page, '$x') === '40',
         'the editor run did not reset the REPL session');

  // --- Test: FizzBuzz example button ---
  console.log('Test: FizzBuzz example');
  await page.click('button:has-text("FizzBuzz")');
  await page.click('#run-btn');
  await page.waitForFunction(() => !document.getElementById('run-btn').disabled, { timeout: 30000 });
  const fbLines = (await page.locator('#repl-log > div').last().textContent()).trim().split('\n');
  assert(fbLines.length === 20, `FizzBuzz has 20 lines (got: ${fbLines.length})`);
  assert(fbLines[0] === '1', `FizzBuzz line 1 = "1" (got: ${JSON.stringify(fbLines[0])})`);
  assert(fbLines[2] === 'Fizz', `FizzBuzz line 3 = "Fizz"`);
  assert(fbLines[4] === 'Buzz', `FizzBuzz line 5 = "Buzz"`);
  assert(fbLines[14] === 'FizzBuzz', `FizzBuzz line 15 = "FizzBuzz"`);

  // --- Test: Ctrl+Enter shortcut ---
  console.log('Test: Ctrl+Enter shortcut');
  await page.fill('#code', 'say 42;');
  await page.press('#code', 'Control+Enter');
  await page.waitForFunction(() => !document.getElementById('run-btn').disabled, { timeout: 30000 });
  assert((await page.locator('#repl-log > div').last().textContent()).trim() === '42',
         'Ctrl+Enter runs the editor buffer');

  // --- Test: Error handling ---
  console.log('Test: Error handling');
  const errOut = await replEval(page, 'die "oops"');
  assert(errOut.includes('Error'), `die produces an error message (got: ${JSON.stringify(errOut)})`);
  assert(await page.locator('#repl-log > div.out.error').count() > 0,
         'error output carries the error CSS class');

  // --- Test: Reset session ---
  console.log('Test: Reset session');
  await page.click('#reset-btn');
  const afterReset = await replEval(page, 'say $x.defined');
  assert(afterReset === 'False', `reset clears declarations (got: ${JSON.stringify(afterReset)})`);

  // --- Test: syntax highlighting ---
  console.log('Test: syntax highlighting');
  await page.fill('#code', 'my $n = 42;  # comment');
  assert(await page.locator('#highlight .tok-keyword').count() > 0, 'keywords are highlighted');
  assert(await page.locator('#highlight .tok-var').count() > 0, 'variables are highlighted');
  assert(await page.locator('#highlight .tok-number').count() > 0, 'numbers are highlighted');
  assert(await page.locator('#highlight .tok-comment').count() > 0, 'comments are highlighted');
  const highlighted = await page.textContent('#highlight');
  assert(highlighted.includes('my $n = 42;  # comment'),
         'the highlight layer reproduces the source verbatim');
  await page.fill('#code', '<script>alert(1)</script>');
  assert(await page.locator('#highlight script').count() === 0,
         'the highlighter escapes HTML rather than injecting it');

  // --- Test: permalink round-trip ---
  console.log('Test: permalink');
  const shared = 'say "unicode: こんにちは";';
  await page.fill('#code', shared);
  await page.evaluate(() => document.getElementById('share-btn').click());
  await page.waitForFunction(() => location.hash.startsWith('#code=') ||
                                   navigator.clipboard !== undefined, { timeout: 5000 });
  const hash = await page.evaluate(() => {
    // Read the link the button produced, whether it went to the clipboard or
    // the address bar.
    const enc = (text) => {
      const bytes = new TextEncoder().encode(text);
      let bin = ''; for (const b of bytes) bin += String.fromCharCode(b);
      return btoa(bin).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
    };
    return enc(document.getElementById('code').value);
  });
  await page.goto(`${BASE}/playground.html#code=${hash}`, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => document.body.dataset.ready === '1', { timeout: 30000 });
  assert(await page.inputValue('#code') === shared,
         'a permalink restores the code, non-ASCII included');
  const sharedOut = await runEditor(page, await page.inputValue('#code'));
  assert(sharedOut === 'unicode: こんにちは',
         `the restored code runs (got: ${JSON.stringify(sharedOut)})`);

  // --- Test: old index.html permalinks still work ---
  console.log('Test: legacy permalink redirect');
  await page.goto(`${BASE}/index.html#code=${hash}`, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => document.body.dataset.ready === '1', { timeout: 30000 });
  assert(new URL(page.url()).pathname.endsWith('/playground.html'),
         'an old index.html permalink redirects to the playground');
  assert(await page.inputValue('#code') === shared, 'and still carries its code');

  // --- Test: No page errors (unreachable traps) ---
  console.log('Test: No WASM crashes');
  assert(errors.length === 0, `No page errors (got ${errors.length}: ${errors.join(', ')})`);

  await browser.close();
} catch (err) {
  console.error('Test runner error:', err.stack || err.message);
  failed++;
} finally {
  server.kill();
  rmSync(BENCH_TSV, { force: true });
  rmSync(BENCH_HTML, { force: true });
}

console.log(`\nResults: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
