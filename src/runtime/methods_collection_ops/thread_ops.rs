//! `Thread` construction, starting (`Thread.start` / `Thread.run`) and
//! joining (`Thread.finish` / `Thread.join`). Split out of `socket_thread`
//! to keep both files under the 500-line limit.

use super::*;
use crate::value::AttrMap;
use crate::value::ValueView;

impl Interpreter {
    /// Thread.start({ block }) -- spawn a real OS thread
    /// Supports named params: :name("..."), :app_lifetime
    pub(in crate::runtime) fn dispatch_thread_start(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract the block and named parameters from args
        let mut block = Value::NIL;
        let mut thread_name = "<anon>".to_string();
        let mut app_lifetime = false;

        for arg in args {
            match arg.view() {
                ValueView::Pair(k, v) if k.as_str() == "name" => {
                    thread_name = v.to_string_value();
                }
                ValueView::Pair(k, v) if k.as_str() == "app_lifetime" => {
                    app_lifetime = v.truthy();
                }
                _ => {
                    block = arg.clone();
                }
            }
        }

        let thread_id = super::next_thread_id();
        super::claim_thread_start(thread_id);
        self.spawn_thread_body(block, thread_id, app_lifetime);

        let mut attrs = HashMap::new();
        attrs.insert("id".to_string(), Value::int(thread_id as i64));
        attrs.insert("name".to_string(), Value::str(thread_name));
        attrs.insert("app_lifetime".to_string(), Value::truth(app_lifetime));
        Ok(Value::make_instance(Symbol::intern("Thread"), attrs))
    }

    /// `Thread.run` — start a `Thread.new`-constructed (not yet started)
    /// thread, returning the invocant (rakudo: `method run(Thread:D:)` returns
    /// `self`).
    ///
    /// Rakudo documents "it is an error to run a thread that has already been
    /// started", and MoarVM enforces that with a VM-level panic; mutsu raises
    /// an ordinary catchable exception instead.
    pub(in crate::runtime) fn dispatch_thread_run(
        &mut self,
        target: &Value,
        attributes: &AttrMap,
    ) -> Result<Value, RuntimeError> {
        let thread_id = attributes
            .get("id")
            .or_else(|| attributes.get("thread_id"))
            .and_then(|v| v.as_int())
            .ok_or_else(|| RuntimeError::new("Thread.run: this Thread has no thread id"))?
            as u64;
        let block = attributes.get("code").cloned().ok_or_else(|| {
            RuntimeError::new("Thread.run: this Thread was created without any code to run")
        })?;
        let app_lifetime = attributes
            .get("app_lifetime")
            .map(|v| v.truthy())
            .unwrap_or(false);
        if !super::claim_thread_start(thread_id) {
            return Err(RuntimeError::new(
                "Thread.run: cannot run a thread that has already been started",
            ));
        }
        self.spawn_thread_body(block, thread_id, app_lifetime);
        Ok(target.clone())
    }

    /// Spawn the OS thread that runs `block`, registering its join handle under
    /// `thread_id` unless the thread is `app_lifetime` (those are killed when
    /// the process's main thread terminates, so nothing ever joins them).
    fn spawn_thread_body(&mut self, block: Value, thread_id: u64, app_lifetime: bool) {
        // A thread spawned *inside* a subtest must keep buffering its output
        // through `shared_thread_output` (as `clone_for_thread` already
        // arranges) rather than writing straight to stdout: its TAP lines are
        // subtest-internal and must be drained (indented) into the subtest by
        // `Thread.finish`, not leaked to the real top-level stream ("tests out
        // of sequence"). Only threads spawned at top level get immediate
        // stdout so their output lands in real chronological order relative
        // to the main thread's direct writes.
        let parent_in_subtest = self.tap.subtest_depth() != 0;
        let mut thread_interp = self.clone_for_thread();
        if !parent_in_subtest {
            thread_interp.set_immediate_stdout(true);
        }
        let mutsu_tid = thread_id as i64;
        // Use the large user-code stack (matches `start {}` / Promise / Supply
        // worker threads): `Thread.start` runs arbitrary user code, and the
        // default ~2-8 MiB thread stack overflows on deep VM nesting (e.g. an
        // async server whose react loop constructs objects whose BUILD re-enters
        // the VM -- HTTP::Server::Tiny). See `USER_THREAD_STACK_SIZE`.
        // Deliberately NOT pooled (ADR-0020 §3.6): a `Thread.start` thread has
        // user-visible identity (`$*THREAD.id`) stable for its whole lifetime.
        let handle = crate::runtime::builtins_system::spawn_user_thread("raku-thread", move || {
            // Set the mutsu thread ID for $*THREAD.id consistency
            super::set_current_mutsu_thread_id(mutsu_tid);
            match thread_interp.call_sub_value(block, vec![], false) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Thread error: {}", e.message);
                }
            }
            // Flush any remaining buffered output from the thread
            let output = std::mem::take(&mut thread_interp.output_sink_mut().output);
            if !output.is_empty() {
                // wasm has no readable process stdout, so leftovers go to the
                // shared buffer `Thread.finish` drains (see `OutputSink::emit`).
                #[cfg(target_arch = "wasm32")]
                if let Some(shared) = thread_interp.output_sink().shared_thread_output.clone() {
                    shared.lock().unwrap().push_str(&output);
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    use std::io::Write;
                    let _ = std::io::stdout().write_all(output.as_bytes());
                    let _ = std::io::stdout().flush();
                }
            }
            let stderr = std::mem::take(&mut thread_interp.output_sink_mut().stderr_output);
            if !stderr.is_empty() {
                use std::io::Write;
                let _ = std::io::stderr().write_all(stderr.as_bytes());
                let _ = std::io::stderr().flush();
            }
        });

        if app_lifetime {
            // For app_lifetime threads, don't store the handle -- the thread
            // will be killed when the main thread exits
            drop(handle);
        } else {
            THREAD_HANDLES.lock().unwrap().insert(thread_id, handle);
        }
    }

    /// Thread.finish -- join the thread (block until it completes)
    pub(in crate::runtime) fn dispatch_thread_finish(
        &mut self,
        attributes: &AttrMap,
    ) -> Result<Value, RuntimeError> {
        let thread_id = attributes
            .get("id")
            .or_else(|| attributes.get("thread_id"))
            .and_then(|v| {
                if let ValueView::Int(i) = v.view() {
                    Some(i as u64)
                } else {
                    None
                }
            })
            .ok_or_else(|| RuntimeError::new("Thread has no thread_id"))?;

        let handle = THREAD_HANDLES.lock().unwrap().remove(&thread_id);
        if let Some(handle) = handle {
            // STW-aware: a thread blocked in `.finish`/join counts as quiescent
            // for the GC's cooperative stop-the-world (it cannot mutate the Gc
            // graph until the join returns).
            crate::gc::block_quiescent(|| handle.join())
                .map_err(|_| RuntimeError::new("Thread panicked"))?;
        }
        // Sync shared variables back to env after thread completes
        self.sync_shared_vars_to_env();
        // Joining a thread is a synchronization point: flush any output the
        // thread buffered into `shared_thread_output` (e.g. TAP lines from a
        // thread spawned inside a subtest, which cannot use immediate stdout —
        // see `dispatch_thread_start`) so it lands in real chronological order
        // now, rather than at some later, possibly out-of-order sync point.
        self.drain_shared_thread_output();
        Ok(Value::TRUE)
    }
}
