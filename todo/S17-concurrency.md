# S17 - Concurrency

Reference: `old-design-docs/S17-concurrency.pod`

Covers promises, channels, supplies, and low-level concurrency primitives.

---

## Design Philosophy

- [ ] Composability of concurrent operations
- [ ] Explicit sync/async boundaries
- [x] Implicit parallelism via hyper operators and junctions

---

## Schedulers

- [ ] `$*SCHEDULER` dynamic variable
- [ ] `ThreadPoolScheduler` implementation
- [ ] `.cue(&code, :in, :at, :every, :times, :stop, :catch)` method
- [ ] `Cancellation` objects from `.cue`

---

## Promises

- [x] `Promise.new` creation (basic)
- [x] `.keep($value)` fulfill
- [ ] `.break($reason)` reject
- [x] `.result` get result (blocking)
- [x] `.status` check state (Planned/Kept/Broken)
- [x] `.then(&callback)` chaining
- [ ] `Promise.start(&code)` factory (run in thread pool)
- [ ] `Promise.in($seconds)` delayed promise
- [ ] `Promise.at($instant)` scheduled promise
- [ ] `Promise.anyof(@promises)` combinator
- [ ] `Promise.allof(@promises)` combinator
- [ ] `await` function

---

## Channels

- [x] `Channel.new` creation (basic)
- [x] `.send($value)` enqueue
- [x] `.receive` dequeue (blocking)
- [ ] `.poll` non-blocking receive
- [x] `.close` close channel
- [x] `.closed` check if closed
- [ ] `.fail($reason)` fail channel
- [ ] `.list` iterate as lazy list

---

## Supplies (Reactive Streams)

### Supplier
- [ ] `Supplier.new` creation
- [ ] `.emit($value)` push value
- [ ] `.done` signal completion
- [ ] `.quit($reason)` signal error
- [ ] `.Supply` get tappable Supply

### Supply
- [ ] `.tap(&emit, :done, :quit)` subscribe
- [ ] Live vs on-demand Supplies
- [ ] Factory: `Supply.from-list(@values)`
- [ ] Factory: `Supply.interval($seconds)`

### Supply Operations
- [ ] `.grep(&filter)`, `.map(&transform)`
- [ ] `.unique`, `.squish`
- [ ] `.batch(:elems, :seconds)`
- [ ] `.throttle($limit, $seconds)`
- [ ] `.merge`, `.zip`, `.zip-latest` combinators
- [ ] `.reduce(&reducer)`
- [ ] `.lines`, `.words`
- [ ] `.start(&transform)` async transform

---

## supply / whenever / react Blocks

- [x] `react { ... }` block (basic)
- [x] `whenever $supply -> $val { ... }` (basic)
- [ ] `supply { ... }` block (creates Supply)
- [ ] Multiple `whenever` blocks in react
- [ ] `done` statement in react
- [ ] `LAST` / `QUIT` / `CLOSE` phasers in react
- [ ] Synchronization guarantees

---

## System Events

- [ ] `signal(SIGINT)` returns Supply of signals
- [ ] IO::Notification and `.watch` for file watching

---

## Proc::Async

- [x] `Proc::Async.new($cmd, @args)` creation (basic)
- [ ] `.stdout` Supply
- [ ] `.stderr` Supply
- [ ] `.stdin` for writing
- [x] `.start` returns Promise
- [ ] `.kill($signal)` send signal
- [ ] `.ready` Promise
- [ ] `.pid` process ID

---

## Threads

- [ ] `Thread.start(&code)` factory
- [ ] `Thread.new(:code, :name)` constructor
- [ ] `.finish` wait for completion
- [ ] `.id`, `.name` properties
- [ ] `$*THREAD` current thread variable
- [ ] `app_lifetime` threads (die with program)

---

## Low-level Primitives

### Atomic Operations
- [ ] `cas($var, $expected, $new)` compare-and-swap
- [ ] Lock-free data structure support

### Locks
- [ ] `Lock.new` creation
- [ ] `.protect(&code)` scoped locking
- [ ] `.lock` / `.unlock` manual locking
- [ ] Reentrant semantics

### Semaphores
- [ ] `Semaphore.new($permits)` creation
- [ ] `.acquire` / `.tryacquire` / `.release`
