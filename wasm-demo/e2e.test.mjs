// E2E tests for the WASM playground using Playwright
// Run: node wasm-demo/e2e.test.mjs
//
// Requires: npm install playwright
// Also requires wasm-demo/pkg/ to be built:
//   wasm-pack build --target web --no-default-features --features wasm
//   mv pkg wasm-demo/pkg

import { chromium } from 'playwright';
import { spawn } from 'child_process';
import { existsSync } from 'fs';

const PORT = 18765;
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

// Check pkg exists
if (!existsSync('wasm-demo/pkg/mutsu.js')) {
  console.error('Error: wasm-demo/pkg/ not found. Build WASM first.');
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

  // --- Test: Page loads and WASM initializes ---
  console.log('Test: WASM initialization');
  await page.goto(`http://localhost:${PORT}/`, { waitUntil: 'networkidle' });
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
  await page.goto(`http://localhost:${PORT}/#code=${hash}`, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => document.body.dataset.ready === '1', { timeout: 30000 });
  assert(await page.inputValue('#code') === shared,
         'a permalink restores the code, non-ASCII included');
  const sharedOut = await runEditor(page, await page.inputValue('#code'));
  assert(sharedOut === 'unicode: こんにちは',
         `the restored code runs (got: ${JSON.stringify(sharedOut)})`);

  // --- Test: No page errors (unreachable traps) ---
  console.log('Test: No WASM crashes');
  assert(errors.length === 0, `No page errors (got ${errors.length}: ${errors.join(', ')})`);

  await browser.close();
} catch (err) {
  console.error('Test runner error:', err.stack || err.message);
  failed++;
} finally {
  server.kill();
}

console.log(`\nResults: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
