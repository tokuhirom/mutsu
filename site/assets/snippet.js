/**
 * A runnable snippet: editor + Run button + output pane, optionally with the
 * expected output recorded in the corpus.  Shared by the landing page and the
 * tutorial so both behave identically.
 */

import { createEditor } from './editor.js';
import { runIsolated, stopIsolated, looksLikeError, isReady, boot } from './runner.js';
import { t } from './i18n.js';

/**
 * @param {HTMLElement} host   container to fill
 * @param {object} opts        {code, expect, minHeight, showExpected}
 */
export function createSnippet(host, opts = {}) {
  host.textContent = '';

  const editorHost = document.createElement('div');
  host.appendChild(editorHost);

  const row = document.createElement('div');
  row.className = 'run-row';

  const runBtn = document.createElement('button');
  runBtn.type = 'button';
  runBtn.className = 'btn run-btn';

  const stopBtn = document.createElement('button');
  stopBtn.type = 'button';
  stopBtn.className = 'btn-ghost stop-btn';
  stopBtn.disabled = true;

  const hint = document.createElement('span');
  hint.className = 'hint';

  const resetBtn = document.createElement('button');
  resetBtn.type = 'button';
  resetBtn.className = 'btn-ghost reset-btn';

  const grow = document.createElement('span');
  grow.className = 'grow';

  const verdict = document.createElement('span');
  verdict.className = 'verdict';

  row.append(runBtn, stopBtn, hint, grow, verdict, resetBtn);
  host.appendChild(row);

  // Shown instead of a run for snippets the WebAssembly build cannot execute.
  const note = document.createElement('p');
  note.className = 'snippet-note';
  note.hidden = true;
  host.appendChild(note);

  const out = document.createElement('div');
  out.className = 'output muted';
  host.appendChild(out);

  let expected = opts.expect || '';
  let noBrowser = !!opts.noBrowser;
  let details = null;
  let summary = null;
  let expectedPre = null;
  if (opts.showExpected) {
    // Built up front and hidden when empty: the widget is reused across
    // lessons, so it must exist before the first setCode() supplies text.
    details = document.createElement('details');
    details.className = 'expected';
    summary = document.createElement('summary');
    expectedPre = document.createElement('pre');
    expectedPre.textContent = expected;
    details.append(summary, expectedPre);
    details.hidden = !expected;
    host.appendChild(details);
  }

  const editor = createEditor(editorHost, {
    code: opts.code || '',
    minHeight: opts.minHeight,
    onRun: run,
  });

  let original = opts.code || '';
  let busy = false;
  let runId = 0;

  async function run() {
    if (busy || noBrowser) return;
    busy = true;
    const currentRun = ++runId;
    runBtn.disabled = true;
    stopBtn.disabled = false;
    runBtn.textContent = isReady() ? t('run.running') : t('run.loading');
    verdict.textContent = '';
    verdict.className = 'verdict';
    const res = await runIsolated(editor.getCode());
    if (currentRun !== runId) return;
    const text = res.output;
    out.classList.remove('muted', 'error');
    if (!text) {
      out.textContent = t('output.none');
      out.classList.add('muted');
    } else {
      out.textContent = text;
      if (res.crashed || looksLikeError(text)) out.classList.add('error');
    }
    if (expected) {
      const ok = text.trim() === expected.trim();
      verdict.textContent = ok ? t('verdict.ok') : t('verdict.differs');
      verdict.classList.add(ok ? 'ok' : 'differs');
    }
    busy = false;
    runBtn.disabled = false;
    stopBtn.disabled = true;
    runBtn.textContent = t('run');
    host.dispatchEvent(new CustomEvent('snippetrun', { bubbles: true, detail: { ok: !!expected && text.trim() === expected.trim() } }));
  }

  function stop() {
    if (!busy) return;
    ++runId;
    stopIsolated();
    busy = false;
    runBtn.disabled = false;
    stopBtn.disabled = true;
    runBtn.textContent = t('run');
    out.className = 'output muted';
    out.textContent = t('run.stopped');
  }

  /**
   * Put the widget into its "cannot run here" state: the recorded native
   * output IS the output, so it is shown as such rather than hidden behind an
   * "Expected output" toggle the reader would have to guess at.
   */
  function paintNoBrowser() {
    runBtn.disabled = true;
    stopBtn.disabled = true;
    runBtn.textContent = t('run');
    hint.textContent = t('no-browser.output');
    note.hidden = false;
    note.textContent = t('no-browser.note');
    verdict.textContent = '';
    verdict.className = 'verdict';
    if (details) details.hidden = true;
    out.className = 'output';
    out.textContent = expected || t('output.none');
  }

  function relabel() {
    if (noBrowser) { paintNoBrowser(); return; }
    if (!busy) runBtn.textContent = t('run');
    hint.textContent = t('run.hint');
    note.hidden = true;
    resetBtn.textContent = t('reset');
    stopBtn.textContent = t('run.stop');
    if (summary) summary.textContent = t('expected.summary');
    if (out.classList.contains('muted') && !out.dataset.ran) out.textContent = t('output.empty');
  }

  runBtn.addEventListener('click', run);
  stopBtn.addEventListener('click', stop);
  resetBtn.addEventListener('click', () => {
    stop();
    editor.setCode(original);
    out.className = 'output muted';
    delete out.dataset.ran;
    out.textContent = t('output.empty');
    verdict.textContent = '';
  });
  host.addEventListener('snippetrun', () => { out.dataset.ran = '1'; });
  window.addEventListener('langchange', relabel);

  relabel();
  out.textContent = t('output.empty');

  // The interpreter is a multi-megabyte download. A page that exists to run
  // code (the tutorial) fetches it up front; a page a visitor may only read
  // (the landing page) waits until they show interest in a snippet, so nobody
  // pays for a runtime they never use.
  if (opts.eagerBoot) {
    boot();
  } else {
    const kick = () => {
      boot();
      editor.textarea.removeEventListener('focusin', kick);
      runBtn.removeEventListener('pointerenter', kick);
    };
    editor.textarea.addEventListener('focusin', kick);
    runBtn.addEventListener('pointerenter', kick);
  }

  return {
    run,
    relabel,
    getCode: editor.getCode,
    setCode(code, expectText, cannotRunHere = false) {
      original = code;
      editor.setCode(code);
      expected = expectText || '';
      noBrowser = cannotRunHere;
      if (expectedPre) expectedPre.textContent = expected;
      if (details) details.hidden = !expected;
      delete out.dataset.ran;
      verdict.textContent = '';
      verdict.className = 'verdict';
      if (noBrowser) {
        paintNoBrowser();
        return;
      }
      runBtn.disabled = false;
      note.hidden = true;
      hint.textContent = t('run.hint');
      out.className = 'output muted';
      out.textContent = t('output.empty');
    },
  };
}
