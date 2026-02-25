// E2E tests for the WASM demo site using Playwright
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

async function waitForExecution(page) {
  await page.waitForFunction(
    () => !document.getElementById('run-btn').disabled,
    { timeout: 30000 }
  );
}

async function runCode(page, code) {
  await page.fill('#code', code);
  await page.click('#run-btn');
  await waitForExecution(page);
  return (await page.textContent('#output')).trim();
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
  await page.waitForFunction(
    () => {
      const output = document.getElementById('output');
      return output && !output.classList.contains('loading');
    },
    { timeout: 30000 }
  );
  const initOutput = await page.textContent('#output');
  assert(
    initOutput.includes('Ready'),
    'WASM module loads and shows ready message'
  );

  // --- Test: say "Hello" ---
  console.log('Test: say "Hello"');
  const helloOutput = await runCode(page, 'say "Hello from mutsu!";');
  assert(helloOutput === 'Hello from mutsu!', `say outputs correctly (got: ${JSON.stringify(helloOutput)})`);

  // --- Test: Arithmetic ---
  console.log('Test: Arithmetic');
  const arithOutput = await runCode(page, 'say 2 + 3;');
  assert(arithOutput === '5', `2 + 3 = 5 (got: ${JSON.stringify(arithOutput)})`);

  // --- Test: FizzBuzz example button ---
  console.log('Test: FizzBuzz example');
  await page.click('button:has-text("FizzBuzz")');
  await page.click('#run-btn');
  await waitForExecution(page);
  const fbOutput = (await page.textContent('#output')).trim();
  const fbLines = fbOutput.split('\n');
  assert(fbLines[0] === '1', `FizzBuzz line 1 = "1" (got: ${JSON.stringify(fbLines[0])})`);
  assert(fbLines[2] === 'Fizz', `FizzBuzz line 3 = "Fizz" (got: ${JSON.stringify(fbLines[2])})`);
  assert(fbLines[4] === 'Buzz', `FizzBuzz line 5 = "Buzz" (got: ${JSON.stringify(fbLines[4])})`);
  assert(fbLines[14] === 'FizzBuzz', `FizzBuzz line 15 = "FizzBuzz" (got: ${JSON.stringify(fbLines[14])})`);

  // --- Test: Ctrl+Enter shortcut ---
  console.log('Test: Ctrl+Enter shortcut');
  await page.fill('#code', 'say 42;');
  await page.press('#code', 'Control+Enter');
  await waitForExecution(page);
  const ctrlOutput = (await page.textContent('#output')).trim();
  assert(ctrlOutput === '42', `Ctrl+Enter runs code (got: ${JSON.stringify(ctrlOutput)})`);

  // --- Test: Error handling ---
  console.log('Test: Error handling');
  const errOutput = await runCode(page, 'die "oops";');
  assert(errOutput.includes('Error'), `die produces an error message (got: ${JSON.stringify(errOutput)})`);
  const outputEl = await page.$('#output');
  const hasErrorClass = await outputEl.evaluate(el => el.classList.contains('error'));
  assert(hasErrorClass, 'Error output has error CSS class');

  // --- Test: No page errors (unreachable traps) ---
  console.log('Test: No WASM crashes');
  assert(errors.length === 0, `No page errors (got ${errors.length}: ${errors.join(', ')})`);

  // --- Test: Reduce example ---
  console.log('Test: Reduce example');
  await page.click('button:has-text("Reduce")');
  await page.click('#run-btn');
  await waitForExecution(page);
  const reduceOutput = (await page.textContent('#output')).trim();
  const reduceLines = reduceOutput.split('\n');
  assert(reduceLines[0] === '55', `[+] 1..10 = 55 (got: ${JSON.stringify(reduceLines[0])})`);
  assert(reduceLines[1] === '120', `[*] 1..5 = 120 (got: ${JSON.stringify(reduceLines[1])})`);

  await browser.close();
} catch (err) {
  console.error('Test runner error:', err.message);
  failed++;
} finally {
  server.kill();
}

console.log(`\nResults: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
