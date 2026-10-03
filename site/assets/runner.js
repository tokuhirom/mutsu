/** Worker-backed WASM lifecycle for the site. */
import { WorkerClient } from './worker-client.js';

let bootPromise = null;
let scratch = null;
let runTail = Promise.resolve();

/** Start loading the WASM module.  Safe to call repeatedly. */
export function boot() {
  if (!bootPromise) {
    if (!scratch) scratch = new WorkerClient();
    const client = scratch;
    bootPromise = client.boot().then(() => {
      if (scratch === client) document.body.dataset.wasmReady = '1';
    });
    // boot() is also kicked off speculatively, with nobody awaiting it yet, so
    // a failure (an offline visitor, or a navigation that aborts the fetch)
    // would otherwise surface as an unhandled promise rejection. Callers that
    // do await it still see the rejection.
    bootPromise.catch(() => {});
  }
  return bootPromise;
}

export function isReady() {
  return document.body.dataset.wasmReady === '1';
}

/** A fresh long-lived session (the REPL page). */
export async function createSession() {
  const session = new WorkerClient();
  await session.boot();
  return session;
}

/** Stop an isolated run and discard its WASM instance. */
export function stopIsolated() {
  scratch?.stop();
  scratch = null;
  bootPromise = null;
  delete document.body.dataset.wasmReady;
}

/** Run a snippet with no state carried over from previous runs. */
export async function runIsolated(code) {
  // A shared scratch interpreter must reset and run one snippet at a time.
  const previous = runTail;
  let release;
  runTail = new Promise(resolve => { release = resolve; });
  await previous;
  let client;
  try {
    const loading = boot();
    client = scratch;
    await loading;
    await client.reset();
    const res = await client.evaluate(code);
    return { output: (res.output || '').replace(/\n+$/, ''), crashed: false };
  } catch (e) {
    if (!client || scratch === client) stopIsolated();
    return { output: `WASM error: ${e.message}`, crashed: true };
  } finally {
    release();
  }
}

/** True when the interpreter reported an error rather than a value. */
export function looksLikeError(output) {
  return /(^|\n)(Error|Runtime error|Parse error):/.test(output);
}
