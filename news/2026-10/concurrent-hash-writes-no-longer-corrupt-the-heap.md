# Concurrent writes to one hash no longer corrupt the heap

Three threads doing `%!hash{$k}++` on one `has %!hash` died with SIGSEGV in about
half the runs, and the runs that finished reported an `.elems` larger or smaller
than the number of distinct keys written (#11701). Method::Protected's own test
hit it, because its `is protected` lock was not in effect.

Four routes were missing ADR-0068's container-structure guard: the plain and the
celled arm of `@a[$i]++`/`%h{$k}++`, `%h{$k}:delete`, and the leaf reads (`.keys`,
`.elems`, `AT-KEY`, ...) racing a structural write. They are guarded now, so a data
race on one hash gives a wrong answer at worst, never a use after free. Iteration
and callback methods (`.sort`, `.map`, `.gist`) on a shared hash are still open and
tracked separately.
