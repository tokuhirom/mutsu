use super::*;

impl std::fmt::Debug for PromiseState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PromiseState")
            .field("status", &self.status)
            .field("result", &self.result)
            .field("output", &self.output)
            .field("stderr_output", &self.stderr_output)
            .field("class_name", &self.class_name)
            .finish()
    }
}

impl SharedPromise {
    /// Returns a raw pointer to the inner Arc data, for use in WHICH identity.
    pub fn arc_ptr(&self) -> *const () {
        Arc::as_ptr(&self.inner) as *const ()
    }

    pub(crate) fn new() -> Self {
        Self::new_with_class(Symbol::intern("Promise"))
    }

    pub(crate) fn new_with_class(class_name: Symbol) -> Self {
        Self {
            inner: Arc::new((
                Mutex::new(PromiseState {
                    status: "Planned".to_string(),
                    result: Value::Nil,
                    output: String::new(),
                    stderr_output: String::new(),
                    class_name,
                    thread_payload: None,
                    waiters: Vec::new(),
                }),
                Condvar::new(),
            )),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn new_kept(result: Value) -> Self {
        Self {
            inner: Arc::new((
                Mutex::new(PromiseState {
                    status: "Kept".to_string(),
                    result,
                    output: String::new(),
                    stderr_output: String::new(),
                    class_name: Symbol::intern("Promise"),
                    thread_payload: None,
                    waiters: Vec::new(),
                }),
                Condvar::new(),
            )),
        }
    }

    pub(crate) fn class_name(&self) -> Symbol {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().class_name
    }

    pub(crate) fn id(&self) -> usize {
        Arc::as_ptr(&self.inner) as usize
    }

    /// Store an opaque payload to be handed off to whoever awaits the
    /// promise. Must be called before `keep`/`break_with` so the awaiter
    /// can observe it via `take_thread_payload`.
    pub(crate) fn set_thread_payload(&self, payload: Box<dyn std::any::Any + Send>) {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.thread_payload = Some(payload);
    }

    pub(crate) fn take_thread_payload(&self) -> Option<Box<dyn std::any::Any + Send>> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.thread_payload.take()
    }

    pub(crate) fn keep(&self, result: Value, output: String, stderr: String) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.status = "Kept".to_string();
        state.result = result.clone();
        state.output = output.clone();
        state.stderr_output = stderr.clone();
        let waiters = std::mem::take(&mut state.waiters);
        cvar.notify_all();
        drop(state);
        Self::dispatch_waiters(waiters, "Kept".to_string(), result, output, stderr);
    }

    /// Try to keep; returns Err(current_status) if already kept/broken.
    pub(crate) fn try_keep(&self, result: Value) -> Result<(), String> {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if state.status != "Planned" {
            return Err(state.status.clone());
        }
        state.status = "Kept".to_string();
        state.result = result.clone();
        let waiters = std::mem::take(&mut state.waiters);
        cvar.notify_all();
        drop(state);
        Self::dispatch_waiters(
            waiters,
            "Kept".to_string(),
            result,
            String::new(),
            String::new(),
        );
        Ok(())
    }

    pub(crate) fn break_with(&self, error: Value, output: String, stderr: String) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.status = "Broken".to_string();
        state.result = error.clone();
        state.output = output.clone();
        state.stderr_output = stderr.clone();
        let waiters = std::mem::take(&mut state.waiters);
        cvar.notify_all();
        drop(state);
        Self::dispatch_waiters(waiters, "Broken".to_string(), error, output, stderr);
    }

    /// Try to break; returns Err(current_status) if already kept/broken.
    pub(crate) fn try_break(&self, error: Value) -> Result<(), String> {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if state.status != "Planned" {
            return Err(state.status.clone());
        }
        state.status = "Broken".to_string();
        state.result = error.clone();
        let waiters = std::mem::take(&mut state.waiters);
        cvar.notify_all();
        drop(state);
        Self::dispatch_waiters(
            waiters,
            "Broken".to_string(),
            error,
            String::new(),
            String::new(),
        );
        Ok(())
    }

    /// Register a callback to run once this promise resolves. If it is
    /// already resolved, the callback runs synchronously on the caller's
    /// thread (matching the existing `.then`/`.andthen`/`.orelse` fast path
    /// for an already-kept/broken promise) and this returns `true`.
    /// Otherwise the callback is queued and this returns `false`: some
    /// future `keep`/`try_keep`/`break_with`/`try_break` call will run every
    /// queued waiter, **in registration order**, on a single dedicated
    /// thread. Registering into this shared, ordered queue (rather than
    /// having each caller spawn its own thread that blocks on `wait()`)
    /// is what makes sibling callbacks on the same promise (e.g. an
    /// independent `.then` alongside an `.andthen` chain) deterministically
    /// ordered instead of racing each other's OS thread wake-up latency.
    pub(crate) fn on_resolve(&self, waiter: PromiseWaiter) -> bool {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if state.status == "Planned" {
            state.waiters.push(waiter);
            false
        } else {
            let status = state.status.clone();
            let result = state.result.clone();
            let output = state.output.clone();
            let stderr = state.stderr_output.clone();
            drop(state);
            waiter(status, result, output, stderr);
            true
        }
    }

    /// Run every queued waiter, in order, on a single dedicated thread (so
    /// resolving a promise never blocks the resolver on arbitrary user
    /// callback code). No-op when there is nothing queued.
    fn dispatch_waiters(
        waiters: Vec<PromiseWaiter>,
        status: String,
        result: Value,
        output: String,
        stderr: String,
    ) {
        if waiters.is_empty() {
            return;
        }
        crate::runtime::builtins_system::spawn_user_thread(move || {
            for waiter in waiters {
                waiter(
                    status.clone(),
                    result.clone(),
                    output.clone(),
                    stderr.clone(),
                );
            }
        });
    }

    /// Check if promise is resolved (Kept or Broken).
    pub(crate) fn is_resolved(&self) -> bool {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().status != "Planned"
    }

    pub(crate) fn status(&self) -> String {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().status.clone()
    }

    pub(crate) fn wait(&self) -> (Value, String, String) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        while state.status == "Planned" {
            state = cvar.wait(state).unwrap();
        }
        (
            state.result.clone(),
            state.output.clone(),
            state.stderr_output.clone(),
        )
    }

    pub(crate) fn result_blocking(&self) -> Value {
        self.wait().0
    }
}

impl PartialEq for SharedPromise {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl SharedChannel {
    /// Returns a raw pointer to the inner Arc data, for use in WHICH identity.
    pub fn arc_ptr(&self) -> *const () {
        Arc::as_ptr(&self.inner) as *const ()
    }

    pub(crate) fn new() -> Self {
        let closed_promise = SharedPromise::new();
        Self {
            inner: Arc::new((
                Mutex::new(ChannelState {
                    queue: std::collections::VecDeque::new(),
                    send_closed: false,
                    drained_closed: false,
                    failure: None,
                    closed_promise,
                    supplier_ids: Vec::new(),
                }),
                Condvar::new(),
            )),
        }
    }

    pub(crate) fn send(&self, value: Value) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if state.send_closed {
            return;
        }
        state.queue.push_back(value);
        cvar.notify_one();
    }

    fn finish_if_drained(state: &mut ChannelState) {
        if state.send_closed && state.queue.is_empty() && !state.drained_closed {
            state.drained_closed = true;
            if let Some(err) = state.failure.clone() {
                let _ = state.closed_promise.try_break(err);
            } else {
                let _ = state.closed_promise.try_keep(Value::Nil);
            }
        }
    }

    pub(crate) fn receive_result(&self) -> Result<Value, Value> {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        loop {
            if let Some(val) = state.queue.pop_front() {
                Self::finish_if_drained(&mut state);
                return Ok(val);
            }
            if state.drained_closed {
                if let Some(err) = state.failure.clone() {
                    return Err(err);
                }
                return Ok(Value::Nil);
            }
            state = cvar.wait(state).unwrap();
        }
    }

    pub(crate) fn poll_result(&self) -> Result<Option<Value>, Value> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if let Some(val) = state.queue.pop_front() {
            Self::finish_if_drained(&mut state);
            return Ok(Some(val));
        }
        if state.drained_closed
            && let Some(err) = state.failure.clone()
        {
            return Err(err);
        }
        Ok(None)
    }

    pub(crate) fn close(&self) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.send_closed = true;
        Self::finish_if_drained(&mut state);
        cvar.notify_all();
    }

    pub(crate) fn fail(&self, error: Value) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.send_closed = true;
        state.failure = Some(error);
        Self::finish_if_drained(&mut state);
        cvar.notify_all();
    }

    pub(crate) fn closed_promise(&self) -> SharedPromise {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().closed_promise.clone()
    }

    pub(crate) fn can_send(&self) -> bool {
        let (lock, _) = &*self.inner;
        !lock.lock().unwrap().send_closed
    }

    pub(crate) fn add_supplier(&self, supplier_id: u64) {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().supplier_ids.push(supplier_id);
    }

    pub(crate) fn supplier_ids(&self) -> Vec<u64> {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().supplier_ids.clone()
    }
}

impl PartialEq for SharedChannel {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}
