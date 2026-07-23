/**
 * WASM lifecycle for the whole site.
 *
 * The module is a few megabytes, so it is loaded once per page and shared by
 * every snippet on it.  `runIsolated` gives each snippet a clean interpreter
 * (the tutorial's lessons must not leak declarations into each other), while
 * `createSession` hands out the long-lived REPL the playground needs.
 */

import init, { Repl } from '../pkg/mutsu.js';

let bootPromise = null;
let scratchRepl = null;

/** Start loading the WASM module.  Safe to call repeatedly. */
export function boot() {
  if (!bootPromise) {
    bootPromise = init().then(() => {
      document.body.dataset.wasmReady = '1';
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

/** A fresh REPL session (playground). */
export async function createSession() {
  await boot();
  return new Repl();
}

/**
 * Run a snippet with no state carried over from previous runs.
 *
 * A Rust panic poisons the wasm instance, so a failed run throws away the
 * scratch interpreter rather than reusing a corrupt one.
 *
 * @returns {Promise<{output: string, crashed: boolean}>}
 */
export async function runIsolated(code) {
  try {
    await boot();
  } catch (e) {
    return { output: `Failed to load the interpreter: ${e.message}`, crashed: true };
  }
  if (!scratchRepl) scratchRepl = new Repl();
  try {
    scratchRepl.reset();
  } catch {
    scratchRepl = new Repl();
  }
  try {
    const res = JSON.parse(scratchRepl.evalBlock(code));
    return { output: (res.output || '').replace(/\n+$/, ''), crashed: false };
  } catch (e) {
    scratchRepl = null;
    return { output: `WASM error: ${e.message}`, crashed: true };
  }
}

/** True when the interpreter reported an error rather than a value. */
export function looksLikeError(output) {
  return /(^|\n)(Error|Runtime error|Parse error):/.test(output);
}
