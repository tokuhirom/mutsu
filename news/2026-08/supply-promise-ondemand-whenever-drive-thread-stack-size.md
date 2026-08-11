# On-demand `supply { whenever <Promise> {...} }.Promise` drive thread now gets the user-code stack

`supply_promise_on_demand` (the `.Promise`/`await` coercion of an on-demand
`supply { ... }` block whose body subscribes via `whenever <Promise>`) drives
the subscription on a background thread so the caller's immediately-returned
`Planned` promise never blocks. That drive thread runs the `whenever` body as
real VM bytecode -- method dispatch, grammar/regex recursion, ordinary sub
recursion, whatever the body does -- exactly like `start {}` / `Thread.start`
worker code. It was spawned with `spawn_gc_helper_thread`, which uses the
default (~2 MiB) thread stack reserved for GC-plumbing threads that run no
user code, instead of `spawn_user_thread`'s 256 MiB user-code stack.

Deep recursion in the whenever body overflowed that stack and crashed the
whole process with SIGSEGV. This was hit on `main` via
`Cro::HTTP::Cookie.from-set-cookie`'s grammar-driven regex parsing, which
recurses fairly deep through `walk_tokens` / LTM alternation candidate
evaluation; both `t/http-session-inmemory.rakutest` and
`t/http-session-persistent.rakutest` from the vendored Cro::HTTP suite died
at rc=139 after two passing subtests. `rust-gdb -batch -ex run -ex bt` on the
crashing thread showed the fault at the very entry of
`Interpreter::parse_regex_uncached` with a corrupted `self` pointer -- the
classic signature of a stack-guard-page hit mid-call, not a real null
dereference.

Fixed by spawning the drive thread with `spawn_user_thread` instead (same GC
registration protocol, just a different stack size). Every other
`spawn_gc_helper_thread` call site in the codebase was re-audited and
confirmed to run no user callback dispatch (raw I/O reads, `Promise.wait()`,
signal/timer plumbing), so this was the only mislabeled site.

Pinned by `t/supply-promise-ondemand-whenever-deep-stack.t`: a self-contained
(Cro-independent) repro that recurses an ordinary sub 600 levels inside a
`whenever $promise { ... }` body coerced via `.Promise` -- deep enough to
overflow a default thread stack but well inside the 256 MiB budget.
Fixing the crash also incidentally resolved `http-session-inmemory`'s
previously-failing "Session expires appropriately" subtest, bringing that
file to a full 13/13.
