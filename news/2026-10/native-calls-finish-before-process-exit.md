# Native calls finish before the process exits (#14864)

The battery gate's `IO::Socket::Async::SSL` files (`thread-stress.rakutest`,
`encoding.rakutest`) died now and then with a SIGSEGV on a `pool` thread inside
`EVP_MD_fetch` (fault address `0x18`, or a stray heap address). The cause was the
exit path, not a native handle shared between threads: a test that ends with an
exception ends the program while worker threads are still handshaking, and
`std::process::exit` runs libc's `exit()`. That executes OpenSSL's `atexit`
cleanup, which frees the default library context under the worker still reading
it. The same file crashed in about one release run in ten before the change and in
none of 150 afterwards.

Every FFI call now holds a `NativeCallGuard` (`src/native_exit_gate.rs`). The new
`mutsu::native_exit_gate::exit_process` closes the gate, waits up to 250 ms for the
calls already in flight to return and only then lets `exit()` run its handlers; a
thread that tries to start a native call afterwards parks. A thread blocked in
native code past the deadline makes the process leave through `_exit` (after
`fflush(NULL)`), skipping the handlers that would race it. `t/nativecall/native-exit-gate.t`
pins the wait with a 240 ms `usleep` in a worker.
