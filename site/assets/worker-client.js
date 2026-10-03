/** An interpreter owned by a dedicated worker, with cancellable requests. */
export class WorkerClient {
  constructor() {
    this.worker = null;
    this.pending = new Map();
    this.nextId = 0;
    this.ready = null;
    this.pendingInput = false;
  }

  boot() {
    if (!this.ready) {
      this.worker = new Worker(new URL('./wasm-worker.js', import.meta.url), { type: 'module' });
      this.worker.onmessage = ({ data }) => {
        const request = this.pending.get(data.id);
        if (!request) return;
        this.pending.delete(data.id);
        if (data.ok) request.resolve(data.result);
        else {
          request.reject(new Error(data.error));
          this.stop(new Error(data.error));
        }
      };
      this.worker.onerror = event => {
        event.preventDefault();
        this.stop(new Error(event.message || 'Worker failed'));
      };
      this.ready = this.request('boot');
      this.ready.catch(() => {});
    }
    return this.ready;
  }

  request(action, code, mode) {
    const id = ++this.nextId;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.worker.postMessage({ id, action, code, mode });
    });
  }

  async evaluate(code, mode = 'block', timeLimitMs = 30_000) {
    await this.boot();
    const timer = setTimeout(() => this.stop(new Error('Execution timed out')), timeLimitMs);
    let result;
    try {
      result = await this.request('evaluate', code, mode);
    } finally {
      clearTimeout(timer);
    }
    this.pendingInput = !!result.incomplete;
    return result;
  }

  async reset() {
    await this.boot();
    await this.request('reset');
    this.pendingInput = false;
  }

  isPending() {
    return this.pendingInput;
  }

  stop(reason = new Error('Execution stopped')) {
    this.worker?.terminate();
    this.worker = null;
    this.ready = null;
    this.pendingInput = false;
    for (const request of this.pending.values()) request.reject(reason);
    this.pending.clear();
  }
}
