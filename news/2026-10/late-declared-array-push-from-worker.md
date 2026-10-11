# A worker's push reaches an array declared after its `start`

```raku
my $s = Supplier.new;
start { sleep 0.05; $s.emit("hi"); $s.done }
my @got;                                   # declared after the start
$s.Supply.tap(-> $t { @got.push($t) });
```

The tap callback runs on the worker that emits. Its `@got.push` used to succeed
there and then disappear: `@got` stayed empty in the declaring frame. Rakudo
prints `["hi"]`. LLM::Chat 0.10.0's `t/07-mock-backend.rakutest` failed 2 of 17
subtests on exactly this shape
([#11343](https://github.com/tokuhirom/mutsu/issues/11343)).

`@got` already had the right home: an `@` lexical captured by a closure that
writes it gets a shared `ContainerRef` cell at its declaration (ADR-0039 /
ADR-0055), held by the frame and by the closure. But once any thread exists,
the method form `@a.push(...)` / `unshift` / `append` / `prepend` took a fast
path into the name-keyed `__mutsu_atomic_arr::` lane. That lane only knows the
names seeded when a worker was spawned, so for a later `@got` the worker made a
private entry. The sub form `push @got, $t` lost the push the same way.

Now a celled binding appends into its cell, under the cell's ADR-0068 structure
lock plus the cell's own mutex. Concurrent appends from many threads still all
land. LLM::Chat's `t/07-mock-backend.rakutest` passes 17/17.

Pin: `t/concurrency/thread-lock/thread-late-declared-array-push.t`.
