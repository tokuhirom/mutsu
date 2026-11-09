# ADR-15967 accepted: mutsu's thread model

ADR-15967 started as "must two back-to-back `start` blocks begin in submission order?" (an
MCP::Server test assumed so) and was accepted as mutsu's thread model in three layers.

**Contract.** A `start` promises what rakudo promises and nothing more: no begin order between
tasks. The MCP::Server `t/21` subtest passes 10/10 on a cold rakudo only because rakudo's pool
starts with a single worker; after warming the pool with 16 `start`s, the unmodified file fails
3/10 on rakudo as well. #15967 is closed as "no ordering guarantee"; the gate branch stays
unmerged as an A/B reference.

**Execution model.** A `start` must cost O(1) in program state. Today a release build takes
231 µs with 300 lexicals in scope (rakudo: 33 µs) and 131 µs after 300 classes (rakudo: 31 µs).
That is because each task runs on a cloned `Interpreter`. The clone also forks the program's
world: a class `EVAL`'d in a task, an `&f.wrap`, or a handle opened in a task is not seen by
other threads. The target is one shared world, a long-lived execution context per pool worker,
and tasks that carry only their closure, promise and dynamic link. #17217 tracks the cost and
#17216 the wrong answers.

**Blocking and scheduling.** mutsu's `await` holds an OS thread: 303 threads at nested depth 300,
against 4 on rakudo. This measurement is recorded as the trigger for the non-blocking-`await`
axis; stackful tasks are the chosen mechanism, and starting that work still needs its own ADR.
Scheduling stays as ADR-0105 set it. Porting rakudo's supervisor remains rejected.
