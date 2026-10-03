# Every thread that runs an interpreter now joins the GC's stop-the-world

The cycle collector's candidate buffer is shared by the whole process, and its
trial deletion is only sound while every other mutator is stopped. The
stop-the-world counted the CLI main thread and `start`/`Promise` workers, but
not a thread that built its own `Interpreter`: an embedder's thread, or the
parallel test threads of `cargo test`. A collect on one such thread saw no
other mutator, skipped the stop, and trial-deleted nodes that another thread
was still dropping. Debug builds aborted with `Gc::drop strong-count
underflow`, intermittently, in the `*_intern_budget` integration tests (#11714).

`Interpreter::new` now registers its thread as a mutator for the rest of the
thread's life, using the same birth protocol as a worker, and a thread-local
guard unregisters it at thread exit after dropping the thread's `Gc`-bearing
thread-locals. `mutsu::gc_register_main_thread` became idempotent, so calling
it on an already registered thread no longer counts the thread twice. A new
integration test runs interpreters on four parallel threads with cyclic
garbage. Without the fix it failed in 5 of 10 runs, and with it in none.
