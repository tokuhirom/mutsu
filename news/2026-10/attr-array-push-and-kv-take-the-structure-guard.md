# Two more unlocked writes to a shared attribute container now take the structure guard

[#11701](https://github.com/tokuhirom/mutsu/issues/11701) found Method::Protected
crashing with SIGSEGV when three threads hammered one `has %!hash`. The test file
no longer crashes on `main`. A wider stress over the same object shape still
crashed the process on two routes. Both mutated a `has` container in place with
nothing held, while other threads reached the same node through the shared
instance:

- `@!a.push(...)` reached `push_to_shared_var`'s in-place fallback. That
  extended the node directly. Six threads pushing to one instance's `@!a`
  aborted with `malloc_consolidate(): unaligned fastbin chunk detected` on 3 of 6
  release runs.
- `%!h.kv` (and the other element-container producers: `.pairs`, `.values`,
  ...) walked the backing map and installed element cells into it, while another
  thread's `%!h{$k}++` rehashed the map. That raised a `capacity overflow` panic.

Both now take the ADR-0068 structure guard on the container node. The
mutating-method funnel and the leaf reads already key an uncelled container that
way. The same stress now runs clean on 6 of 6 runs, and Method::Protected's
`t/01-basic.rakutest` passes its three concurrency tests on every run.

Reads that reach a shared node without passing any guarded funnel can still
crash: numeric coercion (`@!a > 40`), the pre-dispatch proxy scan of a method
receiver, and `.grep` over the live array. That class needs a design, not
another per-site guard, and is filed as
[#12814](https://github.com/tokuhirom/mutsu/issues/12814).

Pin: `t/concurrency/concurrent-attr-array-push-and-kv-iteration.t`.
