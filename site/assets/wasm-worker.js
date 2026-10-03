// Each worker owns its WASM instance. Terminating a worker discards all Rust
// globals as well as an interpreter that may have trapped.
const packaged = !new URL(import.meta.url).pathname.includes('/assets/');
const bindingsUrl = new URL(packaged ? './mutsu.js' : '../pkg/mutsu.js', import.meta.url);

let repl;

self.onmessage = async ({ data }) => {
  const { id, action, code, mode } = data;
  try {
    if (action === 'boot') {
      const bindings = await import(bindingsUrl);
      await bindings.default();
      repl = new bindings.Repl();
      self.postMessage({ id, ok: true });
      return;
    }
    if (action === 'reset') {
      repl.reset();
      self.postMessage({ id, ok: true });
      return;
    }
    const result = JSON.parse(mode === 'line' ? repl.evalLine(code) : repl.evalBlock(code));
    self.postMessage({ id, ok: true, result });
  } catch (error) {
    self.postMessage({ id, ok: false, error: String(error?.message ?? error) });
    self.close();
  }
};
