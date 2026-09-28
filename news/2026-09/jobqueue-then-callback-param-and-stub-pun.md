# Spawned blocks keep their captured parameters; punning a stubbed role dies

Two fixes found by working the `JobQueue` 0.1.1 distribution (drawn at random
by the ecosystem roulette). Three of its four test files timed out under mutsu;
three of the four now pass in full.

**A `.then` callback or `start` block lost its captured parameter.** The
cross-thread shared store is keyed by bare variable name. JobQueue's
`Queue.tick` runs `loop { my $job; ...; self!start($job) }`, and `!start`
registers `$promise.then: -> $p { ...; $job.finish($outcome) }`. The next loop
iteration's `my $job;` published `Any` under the name `job`, and the callback
pulled it over its own capture at its first sync point (`$p.result`), so
`$job.finish` died with "No such method 'finish' for invocant of type 'Any'".
The exception disappeared into the unobserved `.then` promise, and the test
hung on `await $job.completion`.

Two gaps caused this. `Promise.then`/`andthen`/`orelse` cloned the interpreter
without the block (`clone_for_thread`), so the callback's own captured scalars
were never kept off the name lane the way a `start` block's are. They now use
`clone_for_thread_for_block`. That keeps plain scalars only, though, and a
`start` block capturing a parameter that holds an object or a Hash
(`-> $job { start { await $g; $job.id } }`) still read the lane. So
`block_captured_scalars` now also keeps every *readonly* binding live in the
spawning frame off the lane. That covers routine, method and pointy-block
parameters, and extends ADR-0023's for-loop-parameter rule: a binding that
cannot be reassigned has no write for the spawn-time snapshot to miss. The
per-call parameter mask applies only once the shared store is active, and the
first spawn of a program happens before that, so the mask alone missed this
case.

**Punning a role that still requires a method now fails.** `role R { method m
{ ... } }; R.new` must fail with "Method 'm' must be implemented by R because
it is required by roles: R.", as `class C does R { }` does. The pun built its
class without running `resolve_class_stub_requirements`, so the pun succeeded
and only the stub body died ("Stub code executed"). The pun boundary
(`ensure_role_punned_to_class`) now runs that check and withdraws the
half-built pun when it fails.

The remaining file, `t/02-coordinator.rakutest`, is blocked on #10031: a
closure that captured an aggregate parameter and has spawned a thread writes
hash elements into a detached copy. #10032 records a smaller finding from the
same reduction: a lexical `&run`/`&uc` is ignored inside a `.then` callback.
