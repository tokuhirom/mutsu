/**
 * Drop-in <mutsu-code> element for running Raku examples in a web page.
 *
 * The WASM module URL is relative to this file, so the component works both on
 * the mutsu GitHub Pages site and when the complete site/ directory is hosted
 * elsewhere.
 */

// The source file lives under site/assets/, while the same file is copied next
// to mutsu.js in the npm package. Resolve the binding for both layouts so the
// exact component tested on the project site is the one users install.
const packaged = !new URL(import.meta.url).pathname.includes('/assets/');
const bindingsUrl = new URL(packaged ? './mutsu.js' : '../pkg/mutsu.js', import.meta.url);
let Repl;

let wasmReady;

function boot() {
  if (!wasmReady) {
    wasmReady = import(bindingsUrl).then(async bindings => {
      Repl = bindings.Repl;
      await bindings.default();
    });
  }
  return wasmReady;
}

const stylesheet = `
  :host {
    color: #1f2937;
    display: block;
    font: 14px/1.5 ui-sans-serif, system-ui, sans-serif;
    margin: 1rem 0;
  }
  .frame {
    border: 1px solid #cbd5e1;
    border-radius: .5rem;
    overflow: hidden;
  }
  textarea, output {
    box-sizing: border-box;
    display: block;
    font: 14px/1.55 ui-monospace, SFMono-Regular, Consolas, monospace;
    margin: 0;
    padding: .75rem;
    tab-size: 4;
    white-space: pre-wrap;
    width: 100%;
  }
  textarea {
    background: #f8fafc;
    border: 0;
    color: #0f172a;
    min-height: 7rem;
    resize: vertical;
  }
  textarea:focus { outline: 2px solid #2563eb; outline-offset: -2px; }
  .toolbar {
    align-items: center;
    background: #e2e8f0;
    display: flex;
    gap: .5rem;
    padding: .45rem .65rem;
  }
  button {
    background: #fff;
    border: 1px solid #94a3b8;
    border-radius: .3rem;
    color: #0f172a;
    cursor: pointer;
    font: inherit;
    padding: .2rem .65rem;
  }
  button:first-child { background: #2563eb; border-color: #2563eb; color: #fff; }
  button:disabled { cursor: wait; opacity: .65; }
  .status { color: #475569; margin-left: auto; }
  output {
    background: #0f172a;
    color: #e2e8f0;
    min-height: 3rem;
  }
  output.error { color: #fca5a5; }
`;

class MutsuCode extends HTMLElement {
  connectedCallback() {
    if (this.shadowRoot) return;

    const sourceNode = this.querySelector('script[type="text/raku"]');
    const source = sourceNode ? sourceNode.textContent.replace(/^\n|\n\s*$/g, '') : this.textContent.trim();
    this.textContent = '';
    this.session = null;

    const root = this.attachShadow({ mode: 'open' });
    root.innerHTML = `
      <style>${stylesheet}</style>
      <div class="frame">
        <textarea aria-label="Raku code" spellcheck="false"></textarea>
        <div class="toolbar">
          <button type="button" class="run">Run</button>
          <button type="button" class="reset">Reset</button>
          <span class="status" aria-live="polite">Loading mutsu…</span>
        </div>
        <output aria-label="Output">Output will appear here.</output>
      </div>`;

    this.editor = root.querySelector('textarea');
    this.output = root.querySelector('output');
    this.status = root.querySelector('.status');
    this.runButton = root.querySelector('.run');
    this.initialSource = source;
    this.editor.value = source;
    this.editor.readOnly = this.hasAttribute('readonly');
    this.runButton.disabled = true;

    this.runButton.addEventListener('click', () => this.run());
    root.querySelector('.reset').addEventListener('click', () => this.reset());
    this.editor.addEventListener('keydown', event => {
      if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') this.run();
    });

    boot().then(() => {
      this.status.textContent = 'Ready';
      this.runButton.disabled = false;
      if (this.hasAttribute('autorun')) this.run();
    }).catch(error => {
      this.status.textContent = 'Failed to load mutsu';
      this.show(`WASM error: ${error.message}`, true);
    });
  }

  async run() {
    this.runButton.disabled = true;
    this.status.textContent = 'Running…';
    await new Promise(resolve => setTimeout(resolve, 0));
    try {
      await boot();
      if (!this.session || !this.hasAttribute('session')) this.session = new Repl();
      const raw = this.hasAttribute('session')
        ? this.session.evalLine(this.editor.value)
        : this.session.evalBlock(this.editor.value);
      const result = JSON.parse(raw);
      const text = (result.output || '').replace(/\n+$/, '');
      this.show(text || '(no output)', /(^|\n)Error:/.test(text));
      this.status.textContent = 'Ready';
      this.dispatchEvent(new CustomEvent('mutsu-run', {
        bubbles: true,
        detail: { output: text },
      }));
    } catch (error) {
      this.session = null;
      this.show(`WASM error: ${error.message}`, true);
      this.status.textContent = 'Execution failed';
    } finally {
      this.runButton.disabled = false;
    }
  }

  reset() {
    this.editor.value = this.initialSource;
    this.session = null;
    this.show('Output will appear here.', false);
    this.status.textContent = 'Ready';
  }

  show(text, error) {
    this.output.textContent = text;
    this.output.classList.toggle('error', error);
  }
}

if (!customElements.get('mutsu-code')) customElements.define('mutsu-code', MutsuCode);
