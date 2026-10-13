# Chained subscript stores no longer race their own probes

`%h<k>[$i] = 1` and `@a[0][$i] = 1`, run from many threads through a named sub, could crash
the process with `double free or corruption` or `Gc::make_mut strong-count underflow`. This
happened about once in 35 debug runs with 16 threads (#11538, intermittent in CI for months).
Before storing, the op runs a few read-only probes, for example whether the target element is
a `Proxy`. Those probes walked the root and its inner array with no lock held, while another
thread's store resized that inner array in place under the root's ADR-0068 stripe. The probes
now take the same stripe. The repro passes 180 runs out of 180 (ADR-0068 §18.2).
