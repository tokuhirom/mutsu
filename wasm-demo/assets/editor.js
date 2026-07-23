/**
 * The editable code widget: a transparent <textarea> stacked on a highlighted
 * <pre>.  Used by the playground, the tutorial, and the landing-page snippets.
 */

import { highlight } from './highlight.js';

/**
 * Turn an existing `.editor-wrap` (or a fresh one) into an editor.
 *
 * @param {HTMLElement} wrap   element to fill; gets the `editor-wrap` class
 * @param {object} opts        {code, minHeight, onRun, label}
 * @returns {{getCode: () => string, setCode: (s: string) => void,
 *            textarea: HTMLTextAreaElement}}
 */
export function createEditor(wrap, opts = {}) {
  wrap.classList.add('editor-wrap');
  wrap.textContent = '';

  const pre = document.createElement('pre');
  pre.setAttribute('aria-hidden', 'true');

  const textarea = document.createElement('textarea');
  textarea.spellcheck = false;
  textarea.autocapitalize = 'off';
  textarea.autocomplete = 'off';
  textarea.setAttribute('autocorrect', 'off');
  if (opts.label) textarea.setAttribute('aria-label', opts.label);
  textarea.style.minHeight = opts.minHeight || '9rem';

  wrap.appendChild(pre);
  wrap.appendChild(textarea);

  function sync() {
    // A trailing newline needs a filler so the <pre> keeps the textarea height.
    pre.innerHTML = highlight(textarea.value) + '\n';
    pre.scrollTop = textarea.scrollTop;
    pre.scrollLeft = textarea.scrollLeft;
  }

  textarea.addEventListener('input', sync);
  textarea.addEventListener('scroll', () => {
    pre.scrollTop = textarea.scrollTop;
    pre.scrollLeft = textarea.scrollLeft;
  });
  textarea.addEventListener('keydown', (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter' && opts.onRun) {
      e.preventDefault();
      opts.onRun();
    }
  });

  const api = {
    textarea,
    getCode: () => textarea.value,
    setCode: (text) => { textarea.value = text; sync(); },
  };
  api.setCode(opts.code || '');
  return api;
}
